# toyRollup
Exercice

# Les endpoints disponible

Serveur rollup (qui gère le L1 et le L2)
    - /registration -> endpoint appelé par les sequenceur pour s'enregistrer sur un rollup
    - /ledger/new -> création d'une entrée dans le ledger pour un nouveau noeud
    - /rollup/new -> création d'un rollup
    - /rollup/fund -> deposit
    - /rollup/put -> transaction d'un noeud A vers un noeud B dans un rollup
    - /ledger/balance/:identity -> la balance du L1 d'un participant
    - /rollup/:id/balance/:identity -> la balance d'un participant a un rollup
    - /rollup/:id/:identity -> l'existance d'un participant a un rollup (uniquement utilisé par les séquenceurs)
    - /rollup/:id/level -> le niveau courant d'un rollup
    - /rollup/id/participants -> les participants d'un rollup
    - /sequencers -> la liste de séquenceurs et des rollups associés

Serveur séquenceur
    - /sequencer/transaction -> envoyer une transaction a un séquenceur, qui soit la rejete, soit l'accèpte et l'envoie au rollup auquel il est associé (il envoie au serveur rollup quoi)
    - /sequencer/batch -> actuellement ça créer juste un batch mais ça fait rien, je sais pas quoi en faire.