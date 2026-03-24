# League of Legends Match Outcome Prediction
# Machine Learning Classification Project | Note: AI was used to assist with consolidating plots together for conciseness as well as general debugging

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend to avoid tkinter threading issues
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler, LabelEncoder
from sklearn.metrics import (accuracy_score, precision_score, recall_score, 
                             f1_score, roc_auc_score, confusion_matrix, roc_curve)
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
import warnings
import os


np.random.seed(42)
os.makedirs('figures', exist_ok=True)

# LOAD DATA
# Import all CSV tables from the relational database
match_stats = pd.read_csv('MatchStatsTbl.csv')
champions = pd.read_csv('ChampionTbl.csv')
ranks = pd.read_csv('RankTbl.csv')
items = pd.read_csv('ItemTbl.csv')
summoner_match = pd.read_csv('SummonerMatchTbl.csv')
matches = pd.read_csv('MatchTbl.csv')
team_match = pd.read_csv('TeamMatchTbl.csv')

# MERGE TABLES
# Combine all relational tables using foreign keys to create single analysis dataset
df = (match_stats
      .merge(summoner_match, left_on='SummonerMatchFk', right_on='SummonerMatchId', how='left')
      .merge(matches, left_on='MatchFk', right_on='MatchId', how='left')
      .merge(champions, left_on='ChampionFk', right_on='ChampionId', how='left')
      .merge(ranks, left_on='RankFk', right_on='RankId', how='left')
      .merge(team_match, left_on='MatchFk', right_on='MatchFk', how='left'))


# Removing any rows that dont contain lane data as this represents players that werent assigned a role. 
# To me this means games that didn't start or corrupt data as players must be assigned a role to be playing the game 

# Remove records without valid lane assignment
df = df[df['Lane'].isin(['TOP', 'JUNGLE', 'MIDDLE', 'BOTTOM', 'SUPPORT'])]


# FEATURE ENGINEERING
# Create new features from raw data to improve model predictions

# Convert game duration from seconds to minutes for interpretability
df['GameDuration_mins'] = df['GameDuration'] / 60

# Per-minute metrics normalize performance by game length
df['CS_per_min'] = df['MinionsKilled'] / df['GameDuration_mins']
df['Gold_per_min'] = df['TotalGold'] / df['GameDuration_mins']
df['DmgDealt_per_min'] = df['DmgDealt'] / df['GameDuration_mins']
df['DmgTaken_per_min'] = df['DmgTaken'] / df['GameDuration_mins']
df['VisionScore_per_min'] = df['visionScore'] / df['GameDuration_mins']

# Combat metrics measure player effectiveness in fights
df['KDA'] = (df['kills'] + df['assists']) / df['deaths'].replace(0, 1)  # Avoid division by zero
df['Team_Kills'] = np.where(df['Win'] == df['BlueWin'], df['BlueKills'], df['RedKills'])  # Identify player's team
df['Kill_Participation'] = (df['kills'] + df['assists']) / df['Team_Kills'].replace(0, 1)

# Efficiency metrics show how effectively resources are used
df['Dmg_per_Gold'] = df['DmgDealt'] / df['TotalGold'].replace(0, 1)

# Team differentials compare team performance directly
df['Kill_Differential'] = df['BlueKills'] - df['RedKills']
df['Baron_Differential'] = df['BlueBaronKills'] - df['RedBaronKills']
df['Dragon_Differential'] = df['BlueDragonKills'] - df['RedDragonKills']
df['Tower_Differential'] = df['BlueTowerKills'] - df['RedTowerKills']

# Objective metrics quantify strategic map control
df['Objective_Score'] = df['DragonKills'] + (df['BaronKills'] * 2)  # Baron weighted higher
item_cols = ['item1', 'item2', 'item3', 'item4', 'item5', 'item6']
df['Completed_Items'] = df[item_cols].notna().sum(axis=1)  # Count non-empty item slots
df['Mastery_log'] = np.log1p(df['CurrentMasteryPoints'])  # Log transform for normality

# Encode categorical variables into numeric format for ML models
le_lane = LabelEncoder()
df['Lane_Encoded'] = le_lane.fit_transform(df['Lane'].fillna('UNKNOWN'))
le_rank = LabelEncoder()
df['Rank_Encoded'] = le_rank.fit_transform(df['RankName'].fillna('UNRANKED'))

# Clean data by handling infinite values and missing data
df.replace([np.inf, -np.inf], np.nan, inplace=True)  # Replace infinity with NaN
numeric_cols = df.select_dtypes(include=[np.number]).columns
for col in numeric_cols:
    if df[col].isnull().sum() > 0:
        df[col].fillna(df[col].median(), inplace=True)  # Fill missing with median

# VISUALIZATIONS

# Plot 1: Win/Loss Distribution and Rank Distribution
fig, axes = plt.subplots(1, 2, figsize=(14, 5))

# Show class balance in target variable
win_counts = df['Win'].value_counts()
axes[0].bar(['Loss', 'Win'], win_counts.values, color=['red', 'green'], alpha=0.7)
axes[0].set_title('Match Outcomes', fontsize=14)
axes[0].set_ylabel('Number of Matches')
# Calculate percentages of win/loss in plot
total_matches = len(df)

for i, v in enumerate(win_counts.values):
    percentage = (v / total_matches) * 100
    # Add text with both Count and Percentage
    axes[0].text(i, v, f'{v:,}\n({percentage:.1f}%)', ha='center', va='bottom', fontweight='bold')

# Display most common ranks in dataset
rank_counts = df['RankName'].value_counts().head(10)
axes[1].barh(range(len(rank_counts)), rank_counts.values, color='steelblue')
axes[1].set_yticks(range(len(rank_counts)))
axes[1].set_yticklabels(rank_counts.index)
axes[1].set_title('Top 10 Ranks', fontsize=14)
axes[1].set_xlabel('Count')
axes[1].invert_yaxis()

plt.tight_layout()
plt.savefig('figures/01_outcomes_and_ranks.png', dpi=150)
plt.close()

# Plot 2: Feature Distributions by Win/Loss
# Compare how winning and losing players differ across key metrics
fig, axes = plt.subplots(2, 3, figsize=(15, 10))
axes = axes.ravel()
features_to_plot = ['KDA', 'Gold_per_min', 'Kill_Participation', 
                    'CS_per_min', 'VisionScore_per_min', 'Dmg_per_Gold']

for idx, feature in enumerate(features_to_plot):
    wins = df[df['Win'] == 1][feature].dropna()
    losses = df[df['Win'] == 0][feature].dropna()
    
    # Remove extreme outliers for cleaner visualization
    wins = wins[wins < wins.quantile(0.99)]
    losses = losses[losses < losses.quantile(0.99)]
    
    axes[idx].hist(losses, bins=30, alpha=0.6, color='red', label='Loss')
    axes[idx].hist(wins, bins=30, alpha=0.6, color='green', label='Win')
    axes[idx].set_xlabel(feature)
    axes[idx].set_ylabel('Frequency')
    axes[idx].set_title(feature)
    axes[idx].legend()
    axes[idx].grid(alpha=0.3)

plt.tight_layout()
plt.savefig('figures/02_feature_distributions.png', dpi=150)
plt.close()

# Plot 3: Correlation Heatmap
# Show relationships between features and identify multicollinearity
features_for_corr = ['CS_per_min', 'Gold_per_min', 'DmgDealt_per_min', 
                     'KDA', 'Kill_Participation', 'VisionScore_per_min',
                     'Kill_Differential', 'Baron_Differential', 
                     'Dragon_Differential', 'Tower_Differential',
                     'Objective_Score', 'Completed_Items', 'Win']

corr_matrix = df[features_for_corr].corr()

plt.figure(figsize=(12, 10))
sns.heatmap(corr_matrix, annot=True, fmt='.2f', cmap='coolwarm', 
            center=0, square=True, linewidths=1, cbar_kws={"shrink": 0.8})
plt.title('Feature Correlation Matrix', fontsize=14)
plt.tight_layout()
plt.savefig('figures/03_correlation_heatmap.png', dpi=150)
plt.close()

# Plot 4: Win Rate by Lane
# Analyze if certain lanes have higher win rates
lane_stats = df.groupby('Lane').agg({'Win': ['mean', 'count']})
lane_stats.columns = ['WinRate', 'Count']
lane_stats = lane_stats.sort_values('WinRate', ascending=False)

plt.figure(figsize=(10, 6))
plt.bar(range(len(lane_stats)), lane_stats['WinRate'], color='teal', alpha=0.7)
plt.xticks(range(len(lane_stats)), lane_stats.index, rotation=45, ha='right')
plt.ylabel('Win Rate')
plt.xlabel('Lane')
plt.title('Win Rate by Lane')
plt.axhline(y=0.5, color='red', linestyle='--', linewidth=2, label='50% Baseline')
plt.legend()
plt.grid(axis='y', alpha=0.3)

for i, (idx, row) in enumerate(lane_stats.iterrows()):
    plt.text(i, row['WinRate'] + 0.01, f"n={row['Count']:,.0f}", 
            ha='center', fontsize=9)

plt.tight_layout()
plt.savefig('figures/04_winrate_by_lane.png', dpi=150)
plt.close()

# Data Prep for Modelling

# Select features for model training
model_features = [
    'CS_per_min', 'Gold_per_min', 'DmgDealt_per_min', 'DmgTaken_per_min',
    'VisionScore_per_min', 'kills', 'deaths', 'assists', 'KDA',
    'Kill_Participation', 'Dmg_per_Gold', 'Objective_Score',
    'Kill_Differential', 'Baron_Differential', 'Dragon_Differential',
    'Tower_Differential', 'Completed_Items', 'Mastery_log',
    'Lane_Encoded', 'Rank_Encoded'
]

# Separate features (X) from target variable (y)
X = df[model_features].copy()
y = df['Win'].copy()

# Splitting data into 80/20 test train data respectively
# Stratify ensures balanced class distribution in both sets
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y)

# Scale features to have mean=0 and std=1
# Important for distance-based algorithms and gradient descent
scaler = StandardScaler()
X_train_scaled = pd.DataFrame(scaler.fit_transform(X_train), 
                              columns=model_features, index=X_train.index)
X_test_scaled = pd.DataFrame(scaler.transform(X_test), 
                             columns=model_features, index=X_test.index)

print(f"Training samples: {len(X_train):,}")
print(f"Test samples: {len(X_test):,}")
print(f"Features: {len(model_features)}")

# Training the Models
# Train multiple models to compare performance

models = {}
results = []

# Logistic Regression Model (lr)
# Linear model good for baseline and interpretability
lr_model = LogisticRegression(max_iter=1000, random_state=42, n_jobs=-1)
lr_model.fit(X_train_scaled, y_train)
lr_pred = lr_model.predict(X_test_scaled)
lr_proba = lr_model.predict_proba(X_test_scaled)[:, 1]

models['Logistic Regression'] = {'pred': lr_pred, 'proba': lr_proba}
results.append({
    'Model': 'Logistic Regression',
    'Accuracy': accuracy_score(y_test, lr_pred),
    'Precision': precision_score(y_test, lr_pred),
    'Recall': recall_score(y_test, lr_pred),
    'F1-Score': f1_score(y_test, lr_pred),
    'ROC-AUC': roc_auc_score(y_test, lr_proba)
})

# Decision Tree Model (dt)
# Non-linear model that creates interpretable rules
dt_model = DecisionTreeClassifier(max_depth=15, min_samples_split=100, 
                                  min_samples_leaf=50, random_state=42)
dt_model.fit(X_train, y_train)
dt_pred = dt_model.predict(X_test)
dt_proba = dt_model.predict_proba(X_test)[:, 1]

models['Decision Tree'] = {'pred': dt_pred, 'proba': dt_proba, 'model': dt_model}
results.append({
    'Model': 'Decision Tree',
    'Accuracy': accuracy_score(y_test, dt_pred),
    'Precision': precision_score(y_test, dt_pred),
    'Recall': recall_score(y_test, dt_pred),
    'F1-Score': f1_score(y_test, dt_pred),
    'ROC-AUC': roc_auc_score(y_test, dt_proba)
})

# Random Forest Model (rf)
# Ensemble of decision trees reduces overfitting
rf_model = RandomForestClassifier(n_estimators=100, max_depth=15, 
                                  min_samples_split=100, min_samples_leaf=50,
                                  random_state=42, n_jobs=-1, verbose=0)
rf_model.fit(X_train, y_train)
rf_pred = rf_model.predict(X_test)
rf_proba = rf_model.predict_proba(X_test)[:, 1]

models['Random Forest'] = {'pred': rf_pred, 'proba': rf_proba, 'model': rf_model}
results.append({
    'Model': 'Random Forest',
    'Accuracy': accuracy_score(y_test, rf_pred),
    'Precision': precision_score(y_test, rf_pred),
    'Recall': recall_score(y_test, rf_pred),
    'F1-Score': f1_score(y_test, rf_pred),
    'ROC-AUC': roc_auc_score(y_test, rf_proba)
})

# Gradient Boosting Model (gb)
# Sequential ensemble that learns from previous tree errors
gb_model = GradientBoostingClassifier(n_estimators=100, learning_rate=0.1,
                                      max_depth=5, min_samples_split=100,
                                      min_samples_leaf=50, random_state=42, verbose=0)
gb_model.fit(X_train, y_train)
gb_pred = gb_model.predict(X_test)
gb_proba = gb_model.predict_proba(X_test)[:, 1]

models['Gradient Boosting'] = {'pred': gb_pred, 'proba': gb_proba, 'model': gb_model}
results.append({
    'Model': 'Gradient Boosting',
    'Accuracy': accuracy_score(y_test, gb_pred),
    'Precision': precision_score(y_test, gb_pred),
    'Recall': recall_score(y_test, gb_pred),
    'F1-Score': f1_score(y_test, gb_pred),
    'ROC-AUC': roc_auc_score(y_test, gb_proba)
})



# Model Comparison Plots 

# Create results dataframe and save for reference
results_df = pd.DataFrame(results).round(4)
results_df = results_df.sort_values('Accuracy', ascending=False)
results_df.to_csv('model_results.csv', index=False)

# Plot 5: Model Performance Comparison
# Compare all models across multiple evaluation metrics
fig, axes = plt.subplots(2, 2, figsize=(14, 10))
metrics = ['Accuracy', 'Precision', 'Recall', 'F1-Score']
colors = ['steelblue', 'coral', 'seagreen', 'orange']

for idx, (metric, color) in enumerate(zip(metrics, colors)):
    ax = axes[idx // 2, idx % 2]
    sorted_data = results_df.sort_values(metric, ascending=True)
    
    ax.barh(range(len(sorted_data)), sorted_data[metric], color=color, alpha=0.7)
    ax.set_yticks(range(len(sorted_data)))
    ax.set_yticklabels(sorted_data['Model'])
    ax.set_xlabel(metric)
    ax.set_title(f'{metric} Comparison')
    ax.set_xlim(0, 1)
    ax.grid(axis='x', alpha=0.3)
    
    for i, val in enumerate(sorted_data[metric]):
        ax.text(val + 0.01, i, f'{val:.3f}', va='center', fontsize=9)

plt.tight_layout()
plt.savefig('figures/05_model_metrics_comparison.png', dpi=150)
plt.close()

# Plot 6: ROC Curves
# Visualize true positive vs false positive rate tradeoff
plt.figure(figsize=(8, 6))

for model_name, model_data in models.items():
    fpr, tpr, _ = roc_curve(y_test, model_data['proba'])
    auc = roc_auc_score(y_test, model_data['proba'])
    plt.plot(fpr, tpr, label=f'{model_name} (AUC={auc:.3f})', linewidth=2)

plt.plot([0, 1], [0, 1], 'k--', label='Random', linewidth=2)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curves')
plt.legend(loc='lower right')
plt.grid(alpha=0.3)
plt.tight_layout()
plt.savefig('figures/06_roc_curves.png', dpi=150)
plt.close()

# Plot 7: Confusion Matrices
# Show prediction accuracy breakdown for all models
fig, axes = plt.subplots(2, 2, figsize=(12, 10))
axes = axes.ravel()

for idx, (model_name, model_data) in enumerate(models.items()):
    cm = confusion_matrix(y_test, model_data['pred'])
    
    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', ax=axes[idx], 
                cbar=False, square=True)
# Calculate accuracy specifically for this model
    acc = accuracy_score(y_test, model_data['pred'])
    axes[idx].set_title(f'{model_name}\nAccuracy: {acc:.3f}')
    axes[idx].set_xlabel('Predicted')
    axes[idx].set_ylabel('Actual')
    axes[idx].set_xticklabels(['Loss', 'Win'])
    axes[idx].set_yticklabels(['Loss', 'Win'])

plt.tight_layout()
plt.savefig('figures/07_confusion_matrices.png', dpi=150)
plt.close()

# Plot 8: Feature Importance
# Show which features matter most for tree-based models
fig, axes = plt.subplots(1, 3, figsize=(18, 6))

tree_models = ['Decision Tree', 'Random Forest', 'Gradient Boosting']
colors_fi = ['steelblue', 'seagreen', 'coral']

for idx, (model_name, color) in enumerate(zip(tree_models, colors_fi)):
    importance_df = pd.DataFrame({
        'Feature': model_features,
        'Importance': models[model_name]['model'].feature_importances_
    }).sort_values('Importance', ascending=False).head(10)
    
    axes[idx].barh(range(len(importance_df)), importance_df['Importance'], 
                   color=color, alpha=0.7)
    axes[idx].set_yticks(range(len(importance_df)))
    axes[idx].set_yticklabels(importance_df['Feature'])
    axes[idx].set_xlabel('Importance')
    axes[idx].set_title(f'{model_name}\nTop 10 Features')
    axes[idx].invert_yaxis()
    axes[idx].grid(axis='x', alpha=0.3)

plt.tight_layout()
plt.savefig('figures/08_feature_importance.png', dpi=150)
plt.close()

# Plot 9: Rank Performance Analysis
# Evaluate model accuracy across different player ranks
df_with_pred = df.copy()

# 1. Generate Predictions
df_with_pred['RF_Prediction'] = rf_model.predict(X) 

# 2. Determine if the prediction was correct (1 for yes, 0 for no)
df_with_pred['Correct'] = (df_with_pred['Win'] == df_with_pred['RF_Prediction']).astype(int)

# 3. Group by Rank and calculate Accuracy & Count
rank_performance = df_with_pred.groupby('RankName').agg({
    'Correct': 'mean',          # Calculates Accuracy
    'RF_Prediction': 'count'    # Calculates Sample Size
})

# 4. RENAME columns to match what the plotting loop expects ('Accuracy' and 'Count')
rank_performance = rank_performance.rename(columns={
    'Correct': 'Accuracy', 
    'RF_Prediction': 'Count'    # <--- This fixes the KeyError
})

# 5. Sort by Accuracy
rank_performance = rank_performance.sort_values('Accuracy', ascending=False).head(15)

# 6. Plotting
plt.figure(figsize=(12, 8))
bars = plt.barh(range(len(rank_performance)), rank_performance['Accuracy'], 
                color='purple', alpha=0.7)
plt.yticks(range(len(rank_performance)), rank_performance.index)
plt.xlabel('Model Accuracy')
plt.title('Random Forest Accuracy by Rank (Top 15)')

# Add mean line
plt.axvline(x=rank_performance['Accuracy'].mean(), color='red', 
            linestyle='--', linewidth=2, label=f'Mean: {rank_performance["Accuracy"].mean():.3f}')
plt.legend()
plt.grid(axis='x', alpha=0.3)
plt.gca().invert_yaxis()

# Add text labels (This loop causes the error if 'Count' is missing)
for i, (idx, row) in enumerate(rank_performance.iterrows()):
    plt.text(row['Accuracy'] + 0.005, i, f"n={row['Count']:,.0f}", 
             va='center', fontsize=8)

plt.tight_layout()
plt.savefig('figures/09_rank_performance.png', dpi=150)
plt.close()

# Plot 10: Feature Correlation with Target
# Identify which features correlate most strongly with winning
win_correlations = corr_matrix['Win'].drop('Win').abs().sort_values(ascending=False).head(10)

plt.figure(figsize=(10, 6))
colors_corr = ['green' if corr_matrix.loc[feat, 'Win'] > 0 else 'red' 
               for feat in win_correlations.index]
plt.barh(range(len(win_correlations)), win_correlations.values, color=colors_corr, alpha=0.7)
plt.yticks(range(len(win_correlations)), win_correlations.index)
plt.xlabel('Absolute Correlation with Win')
plt.title('Top 10 Features Correlated with Match Outcome')
plt.gca().invert_yaxis()
plt.grid(axis='x', alpha=0.3)

for i, (feat, val) in enumerate(win_correlations.items()):
    actual_corr = corr_matrix.loc[feat, 'Win']
    plt.text(val + 0.01, i, f'{actual_corr:+.3f}', va='center', fontsize=9)

plt.tight_layout()
plt.savefig('figures/10_win_correlations.png', dpi=150)
plt.close()
