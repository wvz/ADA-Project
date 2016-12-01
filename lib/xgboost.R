require(xgboost)
require(Matrix)
require(data.table)
load('python_questions_final_features.RData')

cols <- names(final_features)[-c(1:3,5:6)]
df<-data.table(final_features,keep.rownames=F)[,-c(1:3,5:6)]
sparse_matrix <- sparse.model.matrix(Score~.-1, data = df)
head(sparse_matrix)

score<-df$Score
bst <- xgboost(data = sparse_matrix, label = score, max.depth = 4,
               eta = 1, nthread = 2, nround = 50,objective = "reg:linear")

xgb.cv(data=xgb.DMatrix(sparse_matrix,label=df$Score), max.depth = 4,nfold=10,metrics=list("rmse"),
       eta = 1, nthread = 2, nround = 50,objective = "reg:linear")

importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst, data = sparse_matrix, label = score)
xgb.plot.importance(importance_matrix = importanceRaw,numberOfClusters = c(2:10))
