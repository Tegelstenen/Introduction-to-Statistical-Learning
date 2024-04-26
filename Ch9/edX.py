# ---------- Solutions to question on edX ---------- #
import numpy as np
from sklearn.svm import SVC
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis


def generate_data():
    mean1 = [1, 1, 1, 1, 1, 0, 0, 0, 0, 0]
    mean0 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    cov = np.identity(10)
    X = np.concatenate(
        (
            np.random.multivariate_normal(mean1, cov, 50),
            np.random.multivariate_normal(mean0, cov, 50),
        )
    )
    return X

X_test = generate_data()
error_rate = []
Y = np.concatenate((np.repeat(0, 50), np.repeat(1, 50)))
# svc = SVC(C=10, kernel="linear")
lda = LinearDiscriminantAnalysis()
for _ in range(100):
    X_train = generate_data()
    lda.fit(X_train, Y)
    pred = lda.predict(X_test)
    error_rate.append(np.mean(pred != Y))

print(np.mean(error_rate))