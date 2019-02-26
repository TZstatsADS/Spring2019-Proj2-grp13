import pandas as pd
import operator

def preprocess():
	df = pd.read_csv("../data/Consumer_Airfare_Report__Table_1_-_Top_1_000_Contiguous_State_City-Pair_Markets.csv")

	data = []
	nrows = df.shape[0]
	for i in range(0, nrows):
		year = str(df['Year'].iloc[i])
		if int(year) <= 2007:
			continue

		data.append([])
		j = len(data)-1
		
		start = ''
		end = ''
		data[j].append(df['city1'].iloc[i])
		data[j].append(df['city2'].iloc[i])

		if df['quarter'].iloc[i] == 1:
			start = year + '-01-01'
			end = year + '-04-01'
		elif df['quarter'].iloc[i] == 2:
			start = year + '-04-01'
			end = year + '-07-01'
		elif df['quarter'].iloc[i] == 3:
			start = year + '-07-01'
			end = year + '-10-01'
		else:
			start = year + '-10-01'
			year_num = int(year) + 1
			end = str(year_num) + '-01-01'

		data[j].append(start)
		data[j].append(end)

		data[j].append(df['carrier_lg'].iloc[i])
		data[j].append(df['carrier_low'].iloc[i])

	df2 = pd.DataFrame(data,columns=['city1','city2','start','end','carrier_lg','carrier_low'])

	return df2


if __name__ == "__main__":
	df = preprocess()
	nrows = df.shape[0]

	iterations = ['carrier_lg', 'carrier_low']

	for it in iterations:
		
		dic = dict()
		for i in range(0, nrows):
			city1 = str(df['city1'].iloc[i])
			city2 = str(df['city2'].iloc[i])

			start = str(df['start'].iloc[i])
			end = str(df['end'].iloc[i])

			carrier_lg = str(df[it].iloc[i])

			if tuple([city1, city2]) not in dic:
				dic[tuple([city1, city2])] = []
			dic[tuple([city1, city2])].append(tuple([start, end, carrier_lg]))


		data = []
		for key in dic:

			tmp_list = dic[key]
			tmp_list = sorted(tmp_list, key=operator.itemgetter(0))

			prev = tmp_list[0] #tuple
			carrier_list = []
			carrier_list.append(list(prev))

			for i in range(1, len(tmp_list)):
				cur = tmp_list[i] #tuple
				if prev[1] == cur[0] and prev[2] == cur[2]:
					carrier_list[len(carrier_list)-1][1] = cur[1]
				else:
					carrier_list.append(list(cur))
				prev = tmp_list[i]

			for item in carrier_list:
				data.append([])
				tmp = [key[0], key[1], item[0], item[1], item[2]]
				data[len(data)-1] = data[len(data)-1] + tmp


		df2 = pd.DataFrame(data, columns=['city1','city2','start','end', it])
		df2.to_csv(it+".csv", index=False)
