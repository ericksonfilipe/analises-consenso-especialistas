import sys
import os
from math import log

def acuracia(pasta, abordagens):
    saida_classe = ""
    saida_total = ""
    consenso = carrega_consenso("dados brutos/surveyfamiliaridade@gmail.com.csv")
    for arquivo_abordagem in abordagens:
        abordagem = carrega_abordagem(pasta + "/" + arquivo_abordagem)
        votos_abordagem = []
        votos_consenso = []
        for classe in consenso.keys():
            #para calculo por classe
            nome_abordagem = arquivo_abordagem.replace(".csv", "")
            i_abordagem = abordagem[classe]
            i_consenso = consenso[classe]
            prec = precisao(i_abordagem, i_consenso)
            cob = cobertura(i_abordagem, i_consenso)
            f = f_measure(prec, cob)
            ndcg = NDCG(i_consenso, i_abordagem, 4)
            saida_classe += "{0}|{1}|{2}|{3}|{4}|{5}\n".format(nome_abordagem, classe, prec, cob, f, ndcg)
            #para calculo total
            votos_abordagem += tuplas(classe, abordagem[classe])
            votos_consenso += tuplas(classe, consenso[classe])
        votos_corretos = corretos(votos_abordagem, votos_consenso)
        prec_total = 0.0 if len(votos_abordagem) == 0 else float(votos_corretos)/len(votos_abordagem)
        cob_total = 0.0 if len(votos_consenso) == 0 else float(votos_corretos)/len(votos_consenso)
        f_total = f_measure(prec_total, cob_total)
        saida_total += "{0}|{1}|{2}|{3}\n".format(nome_abordagem, prec_total, cob_total, f_total)
    arq = open("acuracia_por_classe.csv", "w")
    arq.writelines(saida_classe.strip())
    arq.close()

    arq = open("acuracia.csv", "w")
    arq.writelines(saida_total.strip())
    arq.close()

def NDCG(consenso, abordagem, k):
    # O ranking ideal eh o proprio consenso
    idcg_k = DCG_K(consenso, consenso, k)
    ndcg_k = DCG_K(consenso, abordagem, k)
    return ndcg_k/float(idcg_k)

def DCG_K(consenso, abordagem, k):
    ranking = abordagem[:k-1]
    pesos = [k-i for i in range(k)]
    if not ranking:
        return float('NaN')
    dcg = DCG(ranking, consenso, pesos[0], 1)
    for i in range(1, len(ranking)):
        dcg += DCG(ranking, consenso, pesos[i], i+1)
    return dcg

def DCG(ranking, ideal, peso, i):
    rel_i = rel(ranking, ideal, peso, i)
    return rel_i if i <= 1 else rel_i/log(2, i)

def rel(ranking, ideal, peso, i):
    return float(2**peso - 1) if ranking[i-1] in ideal else float(1 - 2**peso)

def corretos(abordagem, consenso):
    count = 0
    for tupla in abordagem:
        if tupla in consenso:
            count += 1
    return count

def tuplas(classe, indicados):
    lista = []
    for indicado in indicados:
        lista.append((classe,indicado))
    return lista

def f_measure(precisao, cobertura):
    if precisao + cobertura == 0:
        return 0.0
    return (2 * precisao * cobertura)/(precisao + cobertura)

def precisao(abordagem, consenso):
    if len(abordagem) == 0:
        return 0.0
    return len([indicado for indicado in abordagem if indicado in consenso])/float(len(abordagem))

def cobertura(abordagem, consenso):
    if len(consenso) == 0:
        return 0.0
    return len([indicado for indicado in consenso if indicado in abordagem])/float(len(consenso))

def carrega_abordagem(arquivo):
    linhas = carrega_arquivo(arquivo)
    abordagem = {}
    for linha in linhas:
        linha_list = linha.split(",")
        classe = recupera_nome_classe(linha_list[0])
        indicacoes = [nome.strip() for nome in linha_list[1:4] if nome != "" or nome != "NA"]
        abordagem[classe] = indicacoes
    return abordagem

def carrega_consenso(arquivo):
    linhas = carrega_arquivo(arquivo)
    #retirando cabecalho
    linhas.pop(0)

    consenso = {}
    for linha in linhas:
        linha_list = linha.split("|")
        classe = recupera_nome_classe(linha_list[0])
        indicacoes = [nome.strip() for nome in linha_list[2].split(",") if nome != ""]
        if indicacoes != []:
            consenso[classe] = indicacoes
    return consenso

def recupera_nome_classe(classe):
    classe = classe.replace(".java", "")
    if "/" in classe:
        classe_list = classe.split("/")
        return classe_list[-1]
    elif "." in classe:
        classe_list = classe.split(".")
        return classe_list[-1]
    return classe

def carrega_arquivo(arquivo):
    arq = open(arquivo, "r")
    linhas = arq.readlines()
    arq.close()
    return linhas

if __name__ == "__main__":
    try:
        pasta = sys.argv[1]
        abordagens = os.listdir(pasta)
    except:
        print "Informe onde estao os arquivos dos resultados das abordagens. Ex: python calcula_acuracia.py ~/mestrado/analises/abordagens/"
        sys.exit(0)
    acuracia(pasta, abordagens)
