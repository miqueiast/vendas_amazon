import pandas as pd
from datetime import datetime, timedelta
import requests
import base64

# Credenciais da API
usuario = "miqueias.silva"
senha = "GRE26Ui"

# Gerar o token Base64
credenciais = f"{usuario}:{senha}"
token_base64 = base64.b64encode(credenciais.encode()).decode()
print(f"Token Base64: {token_base64}")

# Função para gerar datas em um determinado período
def gerar_datas():
    data_inicio = datetime(2025, 2, 1)  # Data inicial fixa
    data_fim = datetime.now() - timedelta(days=1)  # Data final é ontem
    # Gerar uma lista de datas no formato 'YYYY-MM-DD'
    datas = pd.date_range(data_inicio, data_fim).strftime('%Y-%m-%d')
    return pd.DataFrame(datas, columns=['data'])

# Criar DataFrame de todas as datas
df = gerar_datas()
print("DF com todas as datas:")
print(df)

# Criar DataFrame com datas específicas (as que você já possui no SQL)
df_datas_sql = pd.DataFrame({'data': [
    '2025-02-01', '2025-02-02', '2025-02-03', '2025-02-04', '2025-02-05',
    '2025-02-06', '2025-02-07', '2025-02-08', '2025-02-09', '2025-02-10',
    '2025-02-11', '2025-02-12', '2025-02-13', '2025-02-14', '2025-02-15',
    '2025-02-16', '2025-02-17', '2025-02-18', '2025-02-19', '2025-02-20'
]})
print("\nDF com datas específicas:")
print(df_datas_sql)

# Filtrar as datas que estão em df mas não estão em df_datas_sql
df_datas_api = df[~df['data'].isin(df_datas_sql['data'])]
print("\nDF final (datas que não estão em df_datas_sql):")
print(df_datas_api)

# Cabeçalhos da requisição, incluindo o token de autenticação
headers = {
    "Authorization": f"Basic {token_base64}",
    "Content-Type": "application/json"
}

# DataFrame para armazenar os resultados da API
resultados_api = pd.DataFrame()

# Loop para fazer requisições por data
for data in df_datas_api['data']:
    url = f"https://bergasls.painelalter.com/api/v2/countPerHour?dateBegin={data}&dateEnd={data}&eventType=11"
    try:
        print(f"Tentando acessar URL: {url}")
        print(f"Cabeçalhos: {headers}")

        # Fazer a requisição GET com os cabeçalhos
        response = requests.get(url, headers=headers)
        print(f"Status Code: {response.status_code}")

        # Verificar se houve erro 400 ou outro erro
        if response.status_code == 400:
            print(f"Erro 400: {response.text}")  # Detalhes do erro retornado pela API
            continue

        # Levantar exceção para outros erros HTTP (4xx ou 5xx)
        response.raise_for_status()

        # Processar os dados retornados pela API (se a resposta for JSON)
        dados = response.json()
        print(f"Dados obtidos para {data}: {dados}")

        # Adicionar os dados ao DataFrame de resultados (se for uma lista de dicionários)
        if isinstance(dados, list):
            df_dados = pd.DataFrame(dados)
            resultados_api = pd.concat([resultados_api, df_dados], ignore_index=True)
        else:
            print(f"Formato inesperado de dados para {data}: {dados}")

    except requests.exceptions.RequestException as e:
        print(f"Erro ao buscar dados para {data}: {e}")

# Verificar os dados obtidos
print("\nResultados da API:")
print(resultados_api)

# Salvar o DataFrame final em um arquivo CSV
if not resultados_api.empty:
    resultados_api.to_csv("resultados_api.csv", index=False)
    print("\nResultados salvos no arquivo 'resultados_api.csv'")
else:
    print("\nNenhum dado foi obtido da API.")