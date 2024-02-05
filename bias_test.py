import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats.distributions import chi
from itertools import cycle, islice


def run_bias_test_for_regime_switching_garch_1_1():
    # To run the script change the read_excel path to the S&P 500.xlsx path on your local machine
    returns = pd.read_excel("CHANGE THIS PATH", sheet_name="Returns", index_col="Date").sort_index(
        ascending=True)
    # To run the script change the read_excel path to the ML volatility estimation rolling.xlsx path on your local machine
    volatility = pd.read_excel("CHANGE THIS PATH").sort_index(ascending=False)
    returns_with_forecast = returns.iloc[2500:len(returns) - 2]
    volatility.index = returns_with_forecast.index
    full_frame = pd.concat([returns_with_forecast, volatility], axis=1)
    full_frame.rename({"S&P 500": "S&P 500 Възвръщаемост", "res": "Волатилност"}, axis=1, inplace=True)
    plt.style.use('grayscale')
    my_colors = list(islice(cycle(['gray', 'black']), None, len(full_frame)))
    full_frame.plot(figsize=(15, 10), color=my_colors)
    plt.xlabel("")
    plt.show()
    plt.close()
    z_scores = full_frame["S&P 500 Възвръщаемост"] / full_frame["Волатилност"]
    z_scores.plot.density(figsize=(12, 10))
    plt.title("Z-scores за режимо-превключващ GARCH(1,1) модел")
    plt.ylabel("Плътност")
    plt.show()
    test_statistic = np.sqrt((((z_scores - z_scores.mean()) ** 2).sum()) / len(z_scores - 1))
    # constructing 95% confidence level interval for the test_statistic
    alpha = 0.05
    left = (chi.ppf(alpha / 2, df=len(z_scores) - 1)) / np.sqrt(len(z_scores) - 1)
    right = (chi.ppf(1 - (alpha / 2), df=len(z_scores) - 1)) / np.sqrt(len(z_scores) - 1)
    print("Interval {}% :({}, {})".format((1 - alpha) * 100, left, right))
    print("test statistic: {}".format(test_statistic))


def run_bias_test(full_frame, type):
    my_colors = list(islice(cycle(['gray', 'black']), None, len(full_frame)))
    full_frame.rename({"S&P 500": "S&P 500 Възвръщаемост", type: "Волатилност"}, axis=1, inplace=True)
    plt.style.use('grayscale')
    full_frame.plot(figsize=(15, 10), color=my_colors)
    plt.xlabel("")
    plt.show()
    plt.close()
    z_scores = full_frame["S&P 500 Възвръщаемост"] / full_frame["Волатилност"]
    z_scores.plot.density(figsize=(12, 10))
    plt.title("Z-scores за {} модел".format(type))
    plt.ylabel("Плътност")
    plt.show()
    test_statistic = np.sqrt((((z_scores - z_scores.mean()) ** 2).sum()) / len(z_scores - 1))
    # constructing 95% confidence level interval for the test_statistic
    alpha = 0.05
    left = (chi.ppf(alpha / 2, df=len(z_scores) - 1)) / np.sqrt(len(z_scores) - 1)
    right = (chi.ppf(1 - (alpha / 2), df=len(z_scores) - 1)) / np.sqrt(len(z_scores) - 1)
    print("Interval {}% :({}, {})".format((1 - alpha) * 100, left, right))
    print(type)
    print("test statistic: {}".format(test_statistic))


def plot_s_p_50():
    # To run the script change the read_excel path to the S&P 500.xlsx path on your local machine
    returns = pd.read_excel("CHANGE THIS PATH", sheet_name="Returns", index_col="Date").sort_index(
        ascending=True)
    returns_with_forecast = returns.iloc[6817:]
    plt.style.use('grayscale')
    returns_with_forecast.plot(figsize=(12, 10))
    plt.title("Възвръщаемост на S&P 500")
    plt.show()


def read_simpler_models_volatility():
    # To run the script change the read_excel path to the simpler models volatility.xlsx path on your local machine
    data = pd.read_excel("CHANGE THIS PATH", index_col="Date")
    return data


def load_bias_test_zscores():
    # To run the script change the read_excel path to the bias_zscores.xlsx path on your local machine
    return pd.read_excel("CHANGE THIS PATH", index_col="Date")


def run_rolling_bias(tw):
    data = load_bias_test_zscores().sort_index(ascending=True)

    result_frame = pd.DataFrame(index=data.index,
                                columns=["Класическа оценка", "EWMA", "GARCH", "Режимо-превключващ GARCH"])
    bounds_frame = pd.DataFrame(index=data.index, columns=["95% долна", "95% горна"])
    alpha = 0.05
    left_95 = (chi.ppf(alpha / 2, df=60 - 1)) / np.sqrt(60 - 1)
    right_95 = (chi.ppf(1 - (alpha / 2), df=60 - 1)) / np.sqrt(60 - 1)

    for t in range(0, len(data) - tw + 1):
        sliced_data = data.iloc[t: tw + t]
        t_statistics = sliced_data.apply(lambda x: np.sqrt((((x - np.mean(x)) ** 2).sum()) / 59))
        result_frame.loc[sliced_data.index[-1]]["Класическа оценка"] = t_statistics["Classical"]
        result_frame.loc[sliced_data.index[-1]]["EWMA"] = t_statistics["EWMA"]
        result_frame.loc[sliced_data.index[-1]]["GARCH"] = t_statistics["GARCH"]
        result_frame.loc[sliced_data.index[-1]]["Режимо-превключващ GARCH"] = t_statistics["RS-GARCH"]
        bounds_frame.loc[sliced_data.index[-1]]["95% долна"] = left_95
        bounds_frame.loc[sliced_data.index[-1]]["95% горна"] = right_95

    result_frame.dropna(inplace=True)
    bounds_frame.dropna(inplace=True)

    garch_statistics = result_frame[["GARCH", "Режимо-превключващ GARCH"]]

    plt.style.use('grayscale')
    plt.figure(figsize=(15, 10))
    plt.plot(garch_statistics["GARCH"], linestyle="-.")
    plt.plot(garch_statistics["Режимо-превключващ GARCH"], linestyle="-")
    plt.plot(bounds_frame, color="black")
    plt.legend(["GARCH статистика", "Режимо-превключващ GARCH статистика", "95% доверителен интервал"])
    plt.show()

    statistics_count = len(garch_statistics["GARCH"])

    garch_lower_breaches_count = (garch_statistics["GARCH"] < bounds_frame["95% долна"]).value_counts()[1]
    garch_upper_breaches_count = (garch_statistics["GARCH"] > bounds_frame["95% горна"]).value_counts()[1]
    total_garch_breaches = garch_lower_breaches_count + garch_upper_breaches_count
    total_garch_breaches_percent_of_all = (total_garch_breaches / statistics_count) * 100

    rs_garch_lower_breaches_count = \
        (garch_statistics["Режимо-превключващ GARCH"] < bounds_frame["95% долна"]).value_counts()[1]
    rs_garch_upper_breaches_count = \
        (garch_statistics["Режимо-превключващ GARCH"] > bounds_frame["95% горна"]).value_counts()[1]
    total_rs_garch_breaches = rs_garch_lower_breaches_count + rs_garch_upper_breaches_count
    total_rs_garch_breaches_percent_of_all = (total_rs_garch_breaches / statistics_count) * 100

    print("GARCH breaches {}% of the time".format(np.round(total_garch_breaches_percent_of_all, 2)))
    print("RS GARCH breaches {}% of the time".format(np.round(total_rs_garch_breaches_percent_of_all, 2)))

    pass


if __name__ == "__main__":
    run_bias_test_for_regime_switching_garch_1_1()
    data = read_simpler_models_volatility()
    for column in data.columns:
        if column != "S&P 500":
            run_bias_test(data[["S&P 500", column]], column)

    run_rolling_bias(60)
    pass
