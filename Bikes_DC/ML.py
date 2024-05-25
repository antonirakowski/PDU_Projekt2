import pandas as pd
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.neighbors import KNeighborsRegressor
from sklearn.svm import SVR
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, r2_score
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.linear_model import LinearRegression
first = pd.read_csv("norm_true.csv", header=0)
print(first.head())
target_first=first["casual"].ravel()
features_first = first[["temperature_2m_max...C.","Inne","Inne2","Inne4"]]
x_ftrain , x_ftest, y_fres, y_ftest = train_test_split(features_first,target_first,train_size=0.7,random_state=21377)
Knearest=KNeighborsRegressor(n_neighbors=6)
Knearest.fit(x_ftrain,y_fres)
predictionsK = Knearest.predict(x_ftest)
Gradient = GradientBoostingRegressor(n_estimators=101,random_state=54)
Gradient.fit(x_ftrain,y_fres)
predictionsG = Gradient.predict(x_ftest)
Forest = RandomForestRegressor(n_estimators=102, random_state=56)
Forest.fit(x_ftrain,y_fres)
predictionsF= Forest.predict(x_ftest)
SVR1 = SVR(kernel="rbf")
SVR1.fit(x_ftrain,y_fres)
predictionsS= SVR1.predict(x_ftest)
Line = LinearRegression()
Line.fit(x_ftrain,y_fres)
predictionsL = Line.predict(x_ftest)
errorK1 = mean_squared_error(predictionsK,y_ftest)
errorK2 = r2_score(predictionsK,y_ftest)
errorG1 = mean_squared_error(predictionsG,y_ftest)
errorG2 = r2_score(predictionsG,y_ftest)
errorF1 = mean_squared_error(predictionsF,y_ftest)
errorF2 = r2_score(predictionsF,y_ftest)
errorS1 = mean_squared_error(predictionsS,y_ftest)
errorS2 = r2_score(predictionsS,y_ftest)
errorL1 = mean_squared_error(predictionsL,y_ftest)
errorL2 = r2_score(predictionsL,y_ftest)
print(f"M Error K = {errorK1}, R Error K = {errorK2}")
print(f"M Error G = {errorG1}, R Error G = {errorG2}")
print(f"M Error F = {errorF1}, R Error F = {errorF2}")
print(f"M Error S = {errorS1}, R Error S = {errorS2}")
print(f"M Error L = {errorL1}, R Error L = {errorL2}")

def plot_predictions(y_true, y_pred, title, fname):
  plt.figure(figsize=(8, 8))
  plt.scatter(y_true, y_pred, alpha = 0.5)
  plt.plot([y_true.min(), y_true.max()], [y_true.min(), y_true.max()], "r--",
  lw = 2)
  plt.xlabel("True (normalised) values")
  plt.ylabel("Predicted (normalised) values")
  plt.title(title)
  plt.savefig(fname)

plot_predictions(y_ftest,predictionsG,"Gradient Predictions", "PlotG")
plot_predictions(y_ftest,predictionsL,"Linear Predictions", "PlotL")
plot_predictions(y_ftest,predictionsS,"SVR Predictions", "PlotS")