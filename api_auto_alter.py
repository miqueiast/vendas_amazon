import pandas as pd
from datetime import datetime, timedelta

# Criar uma função para gerar datas em um determinado período
def gerar_datas():
    data_inicio = datetime(2025, 2, 1)
    data_fim = datetime.now() - timedelta(days=1)  # Ontem
    datas = pd.date_range(data_inicio, data_fim).strftime('%Y-%m-%d')
    return pd.DataFrame(datas, columns=['data'])

# Criar DataFrame de todas as datas
df = gerar_datas()
print("DF com todas as datas:")
print(df)

# Criar DataFrame com datas específicas
df_datas_sql = pd.DataFrame({'data': ['2025-02-01', '2025-02-02', '2025-02-03', '2025-02-04', '2025-02-05',
                                     '2025-02-06', '2025-02-07', '2025-02-08', '2025-02-09', '2025-02-10',
                                     '2025-02-11','2025-02-12','2025-02-13','2025-02-14','2025-02-15','2025-02-16'
                                     ,'2025-02-17','2025-02-18','2025-02-19','2025-02-20']})
print("\nDF com datas específicas:")
print(df_datas_sql)

# Criar DataFrame com datas presentes em df e que não estão em df_datas_sql
df_datas_api = df[~df['data'].isin(df_datas_sql['data'])]
print("\nDF final (apenas datas que não estão em df_datas_sql):")
print(df_datas_api)
