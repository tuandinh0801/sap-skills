# SAP HANA ML Skill - Progress Tracking

**Last Updated**: 2025-11-23
**Status**: Complete
**Version**: 2.22.241011 (hana-ml library version)

---

## Documentation Sources

All information extracted from official SAP documentation:
- Base URL: `https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/`

---

## Extraction Status by Source

### Core Documentation

| Source URL | Status | Key Information Extracted |
|------------|--------|---------------------------|
| `hana_ml.html` | EXTRACTED | Library overview, module structure, version 2.22.241011 |
| `Installation.html` | EXTRACTED | Prerequisites, pip installation, HANA requirements |
| `Tutorials.html` | EXTRACTED | Tutorial index, PAL/APL examples, end-to-end workflows |
| `change_log.html` | EXTRACTED | Version history reference (details in main docs) |

### DataFrame Module

| Source URL | Status | Key Information Extracted |
|------------|--------|---------------------------|
| `hana_ml.dataframe.html` | FULLY EXTRACTED | ConnectionContext class (all methods), DataFrame class (all properties/methods), utility functions |

**Extracted Classes:**
- ConnectionContext: `close()`, `get_connection_id()`, `restart_session()`, `cancel_session_process()`, `create_schema()`, `has_schema()`, `get_current_schema()`, `create_table()`, `drop_table()`, `has_table()`, `get_tables()`, `create_virtual_table()`, `get_procedures()`, `drop_procedure()`, `get_temporary_tables()`, `clean_up_temporary_tables()`, `hana_version()`, `hana_major_version()`, `is_cloud_version()`, `sql()`, `execute_sql()`, `table()`, `explain_plan_statement()`, `copy_to_data_lake()`, `to_sqlalchemy()`
- DataFrame: All properties (`columns`, `shape`, `name`, `quoted_name`, `description`, `geometries`, `srids`, `stats`) and methods (selection, filtering, transformation, aggregation, advanced operations)
- Utility Functions: `quotename()`, `read_pickle()`, `create_dataframe_from_pandas()`, `create_dataframe_from_spark()`, `create_dataframe_from_shapefile()`, `melt()`, `import_csv_from()`

### Algorithm Modules

| Source URL | Status | Key Information Extracted |
|------------|--------|---------------------------|
| `hana_ml.algorithms.apl.html` | FULLY EXTRACTED | All APL classes and methods |
| `hana_ml.algorithms.pal.html` | FULLY EXTRACTED | All PAL categories and algorithms |

**APL Classes Extracted:**
- AutoClassifier, GradientBoostingClassifier, GradientBoostingBinaryClassifier
- AutoRegressor, GradientBoostingRegressor
- AutoTimeSeries
- AutoUnsupervisedClustering, AutoSupervisedClustering
- Common methods: `fit()`, `predict()`, `score()`, `fit_predict()`, `save_model()`, `load_model()`, `save_artifact()`, `is_fitted()`, `get_performance_metrics()`, `get_feature_importances()`, `get_debrief_report()`, `get_summary()`, `export_apply_code()`, `set_scale_out()`, `schedule_fit()`, `schedule_predict()`, `set_shapley_explainer_of_predict_phase()`, `build_report()`, `generate_html_report()`

**PAL Categories Extracted:**
- PAL Base: `PALBase`
- AutoML: `AutomaticClassification`, `AutomaticRegression`, `AutomaticTimeSeries`, `Preprocessing`, `MassiveAutomaticClassification`, `MassiveAutomaticRegression`, `MassiveAutomaticTimeSeries`
- Unified Interface: `UnifiedClassification`, `UnifiedRegression`, `UnifiedClustering`, `UnifiedExponentialSmoothing`
- Clustering: `AffinityPropagation`, `AgglomerateHierarchicalClustering`, `DBSCAN`, `GeometryDBSCAN`, `KMeans`, `KMedians`, `KMedoids`, `SpectralClustering`, `KMeansOutlier`, `GaussianMixture`, `SOM`, `SlightSilhouette`
- Classification: `LinearDiscriminantAnalysis`, `LogisticRegression`, `OnlineMultiLogisticRegression`, `NaiveBayes`, `KNNClassifier`, `MLPClassifier`, `SVC`, `OneClassSVM`, `DecisionTreeClassifier`, `RDTClassifier`, `HybridGradientBoostingClassifier`, `MLPMultiTaskClassifier`
- Regression: `LinearRegression`, `OnlineLinearRegression`, `KNNRegressor`, `MLPRegressor`, `PolynomialRegression`, `GLM`, `ExponentialRegression`, `BiVariateGeometricRegression`, `BiVariateNaturalLogarithmicRegression`, `CoxProportionalHazardModel`, `SVR`, `DecisionTreeRegressor`, `RDTRegressor`, `HybridGradientBoostingRegressor`, `MLPMultiTaskRegressor`
- Preprocessing: `FeatureNormalizer`, `FeatureSelection`, `IsolationForest`, `KBinsDiscretizer`, `Imputer`, `Discretize`, `MDS`, `SMOTE`, `SMOTETomek`, `TomekLinks`, `Sampling`, `ImputeTS`, `PowerTransform`, `QuantileTransform`, `OutlierDetectionRegression`, `PCA`, `CATPCA`, `train_test_val_split`, `variance_test`
- Time Series: `ARIMA`, `AutoARIMA`, `AdditiveModelForecast`, `CPD`, `BCPD`, `OnlineBCPD`, `BSTS`, `TimeSeriesClassification`, exponential smoothing variants, `GARCH`, `Hierarchical_Forecast`, `LR_seasonal_adjust`, `LSTM`, `LTSF`, `OnlineARIMA`, `OutlierDetectionTS`, `GRUAttention`, `ROCKET`, `VectorARIMA`, `DWT`, utility functions
- Statistics: Random distributions, hypothesis tests, univariate analysis, distribution fitting, Kaplan-Meier, KDE
- Association: `Apriori`, `AprioriLite`, `FPGrowth`, `KORD`, `SPM`
- Recommender: `ALS`, `FRM`, `FFMClassifier`, `FFMRegressor`, `FFMRanker`, `MLPRecommender`
- Social Network: `LinkPrediction`, `PageRank`, `SVRanking`
- Miscellaneous: `abc_analysis`, `weighted_score_table`, `create_model_card`, `parse_model_card`, `TSNE`, `FairMLClassification`, `FairMLRegression`
- Model Evaluation: `accuracy_score`, `auc`, `confusion_matrix`, `multiclass_auc`, `r2_score`, `binary_classification_debriefing`
- Model Selection: `ParamSearchCV`, `GridSearchCV`, `RandomSearchCV`, `Pipeline`
- Text Processing: `CRF`, `LatentDirichletAllocation`

### Visualizers Module

| Source URL | Status | Key Information Extracted |
|------------|--------|---------------------------|
| `hana_ml.visualizers.html` | FULLY EXTRACTED | All visualizer classes and methods |

**Visualizer Submodules Extracted:**
- `hana_ml.visualizers.eda`: `EDAVisualizer`, `Profiler`, plotting functions (`quarter_plot`, `seasonal_plot`, `timeseries_box_plot`, etc.)
- `hana_ml.visualizers.metrics`: `MetricsVisualizer`, `plot_confusion_matrix()`
- `hana_ml.visualizers.model_debriefing`: `TreeModelDebriefing`, `shapley_explainer()`
- `hana_ml.visualizers.shap`: `ShapleyExplainer`, `TimeSeriesExplainer`
- `hana_ml.visualizers.m4_sampling`: `m4_sampling()`, index functions
- `hana_ml.visualizers.dataset_report`: `DatasetReportBuilder`
- `hana_ml.visualizers.unified_report`: `UnifiedReport`
- `hana_ml.visualizers.digraph`: `Digraph`, `MultiDigraph`, `Node`, `Edge`
- `hana_ml.visualizers.word_cloud`: `WordCloud`
- `hana_ml.visualizers.time_series_report`: `TimeSeriesReport`, `DatasetAnalysis`
- `hana_ml.visualizers.automl_progress`: `PipelineProgressStatusMonitor`, `SimplePipelineProgressStatusMonitor`
- `hana_ml.visualizers.automl_report`: `BestPipelineReport`

### Supporting Modules

| Source URL | Status | Key Information Extracted |
|------------|--------|---------------------------|
| `hana_ml.ml_exceptions.html` | PARTIAL | Module exists (details in reference) |
| `hana_ml.model_storage.html` | EXTRACTED | `save_model()`, `load_model()`, `save_artifact()` methods |
| `hana_ml.artifacts.html` | EXTRACTED | Artifact management integration |
| `hana_ml.docstore.html` | PARTIAL | Document store capability referenced |
| `hana_ml.spatial.html` | EXTRACTED | `geometries`, `srids` properties, `GeometryDBSCAN`, `create_dataframe_from_shapefile()` |
| `hana_ml.graph.html` | EXTRACTED | Graph creation, node/edge management, visualization |
| `hana_ml.graph.algorithms.html` | PARTIAL | Graph algorithms module referenced |
| `hana_ml.text.tm.html` | EXTRACTED | `LatentDirichletAllocation`, `CRF`, `WordCloud` |
| `hana_ml.hana_scheduler.html` | PARTIAL | Scheduler module referenced |

---

## Information Coverage Summary

### Fully Documented in Skill

| Component | Coverage | Notes |
|-----------|----------|-------|
| Installation & Setup | 100% | Prerequisites, pip install, connection |
| DataFrame Operations | 100% | All classes, methods, properties |
| APL Algorithms | 100% | All classification, regression, time series, clustering |
| PAL Algorithms | 100% | All 15+ categories with 100+ algorithms |
| Visualizers | 100% | All 11 submodules |
| Model Storage | 90% | Core methods documented |
| Spatial Features | 80% | Properties and key functions |
| Graph Features | 70% | Core operations documented |
| Text Mining | 70% | Topic modeling and word cloud |
| Scheduler | 50% | Module referenced |
| Exceptions | 50% | Module referenced |

### Templates Included

- [x] Basic connection setup
- [x] DataFrame creation and manipulation
- [x] PAL classification example
- [x] PAL regression example
- [x] APL AutoML example
- [x] Time series forecasting
- [x] Clustering example
- [x] Model persistence
- [x] Visualization example

---

## Update Instructions

To update this skill when new hana-ml versions are released:

1. **Check version**: Visit `https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/latest/en-US/hana_ml.html`
2. **Review changelog**: Check `change_log.html` for new features
3. **Update SKILL.md**: Modify version numbers and add new capabilities
4. **Update references**: Add new algorithm classes or methods to reference files
5. **Test examples**: Verify all code templates work with new version
6. **Update dates**: Change "Last Verified" dates in all files

---

## Source Links for Updates

### Primary Documentation
- Main: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.html
- Installation: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/Installation.html
- Changelog: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/change_log.html

### Module Documentation
- DataFrame: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.dataframe.html
- APL: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.algorithms.apl.html
- PAL: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.algorithms.pal.html
- Visualizers: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.visualizers.html

### Extended Modules
- Model Storage: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.model_storage.html
- Artifacts: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.artifacts.html
- Spatial: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.spatial.html
- Graph: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.graph.html
- Graph Algorithms: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.graph.algorithms.html
- Text Mining: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.text.tm.html
- DocStore: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.docstore.html
- Scheduler: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.hana_scheduler.html
- Exceptions: https://help.sap.com/doc/1d0ebfe5e8dd44d09606814d83308d4b/2.0.07/en-US/hana_ml.ml_exceptions.html

### PyPI Package
- https://pypi.org/project/hana-ml/

---

**Document Version**: 1.0
**Created**: 2025-11-23
