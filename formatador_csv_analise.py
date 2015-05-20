abordagens = ["ranking_commit.csv", "ranking_linha.csv"]

def formata(arquivo):
    arq = open('dados brutos/' + arquivo, 'r')
    linhas = arq.readlines()
    arq.close()

    especialistas = {}

    for linha in linhas:
        classe, dev, contrib = linha.split(',')
        if especialistas.has_key(classe):
            especialistas[classe].append(dev.strip())
        else:
            especialistas[classe] = [dev.strip()]

    saida = ""
    for classe in especialistas.keys():
        ranking = ','.join(especialistas[classe])
        saida += "{0},{1}\n".format(classe, ranking)
    arq = open("abordagens/" + arquivo, "w")
    arq.writelines(saida)
    arq.close()

for abordagem in abordagens:
    formata(abordagem)
