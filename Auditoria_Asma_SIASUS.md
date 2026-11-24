# Auditoria metodológica — `Asma.R` (asma no SIASUS, Alagoas, 2015–2025)

Auditoria do script em três frentes (epidemiologia, estatística e metodologia), conferida contra os próprios dados extraídos (caches PA/BI/AM e `Asma_AL_resultados.rds`). Os achados estão em ordem de prioridade. Cada item traz o problema, a evidência nos dados, o impacto e o ajuste proposto.

**Legenda:** 🔴 crítico (muda resultados ou conclusões) · 🟠 importante (viés relevante) · 🟡 melhoria

---

## Resumo executivo

| # | Achado | Gravidade | Onde |
|---|---|---|---|
| 1 | O filtro "APAC aprovada" apaga meses inteiros de dispensação (2021 e 2º semestre de 2025) e fabrica "interrupções" de tratamento | 🔴 | 9b, 9e |
| 2 | 1.889 "atendimentos APAC" são, na verdade, APAC de medicamento que escaparam da exclusão | 🔴 | 3 |
| 3 | O % de urgência reflete o tipo de estabelecimento que notifica, não a gravidade: a tendência de 1% → 72% é artefato | 🔴 | 5, 8, 9d, 10 |
| 4 | Testes e IC tratam atendimentos repetidos do mesmo paciente como independentes | 🔴 | 5, 6 |
| 5 | A "etapa de tratamento" é inferida pela apresentação do produto, não pela dose | 🟠 | 9c, 9e |
| 6 | APAC "inicial" (AP_TPAPAC = 1) não marca o início do tratamento | 🟠 | 9b |
| 7 | "Atendimento" mistura consultas, pronto-atendimento e sessões de fisioterapia | 🟠 | 3, 9 |
| 8 | Sem denominador populacional: não há taxas, só contagens e proporções | 🟠 | 8, 10 |
| 9 | A cobertura do SIA mudou no período (APS → SISAB, COVID, entrada e saída de unidades) | 🟠 | todo |
| 10 | Antes × depois e BI × AM sem grupo de comparação e confundidos por idade e serviço | 🟠 | 9d, 9e |
| 11–20 | Itens menores (gestantes no IMC, censura à esquerda, testes múltiplos, reprodutibilidade…) | 🟡 | vários |

---

## 🔴 1. O filtro de aprovação cria lacunas falsas no tratamento (AM)

**Problema.** O AM é restrito às APAC com `PA_INDICA` 5/6 no PA (`am3a`, l. 729). A aprovação de APAC de medicamento, porém, oscila por motivos administrativos (faturamento, teto, processamento), não clínicos.

**Evidência** (APAC de medicamento no PA, % aprovadas por competência):

| Competência | APAC | % aprovadas |
|---|---|---|
| 2021-03 a 2021-07 | 1.472 → 144 | 6,9% → 0,0% |
| 2021-11 / 2021-12 | 923 / 294 | 7,3% / 2,7% |
| 2025-02 / 2025-03 | 1.287 / 869 | 20% / 3% |
| 2025-10 a 2025-12 | 1.085 → 240 | 11,8% → 12,5% |

No total, 32.913 registros (16%) saem por esse filtro. Em 2021, 51% são excluídos.

**Impacto.**
- **Seção 9e:** com quase nada aprovado em out–dez/2025, quase ninguém fica "em uso no fim dos dados" (1,2% para budesonida 400). Por isso, 83% dos episódios aparecem como "interrompeu". **Esse resultado é, em grande parte, artefato.**
- **Persistência (`Persistencia_Pct`) e duração:** são subestimadas pelos buracos de 2021.
- **Pacientes por ano:** a queda de 2021 é, em parte, administrativa.

**Ajuste.**
- Para **dispensação e continuidade do tratamento** (persistência, episódios, desfechos), usar **todas as APAC do AM** (`am3`), porque a dispensação aconteceu mesmo quando a APAC foi glosada. Manter o filtro de aprovação só para **produção e valores**.
- Definir o fim dos dados como o último mês com volume **completo**, não `max(AP_CMP)`. Por exemplo, considerar só competências com ≥ 80% do volume mediano e cortar 2025-10+ das análises de desfecho.
- Fazer uma sensibilidade com e sem o filtro e reportar as duas no texto.

---

## 🔴 2. APAC de medicamento vazando para "atendimentos"

**Problema.** As chaves de exclusão (`chaves_apac_med`, l. 390) só usam a linha **"P" aprovada** com procedimento 06. Quando a linha P não foi aprovada, ou não carrega CID de asma, mas uma linha "S" foi, a APAC permanece em `atend`.

**Evidência.** Todos os 5 procedimentos mais frequentes das 1.889 "APAC" restantes em `atend` são **medicamentos** (0604040040 formoterol + budesonida, 0604840020 mepolizumabe…). Há 2.187 linhas "S" aprovadas com procedimento 06.

**Impacto.** O perfil "APAC" em `indicadores_instrumento` (69,7% feminino, 3,6% < 20 anos, 0% urgência) é o perfil do AM, não de atendimento. Os picos de APAC em 2020 (573) e 2023 (440) vêm disso. Também contamina `Instr_Posicao` e o teste de posição do CID.

**Ajuste.**
- Montar as chaves a partir de `pa3` (antes do filtro de aprovação), com **qualquer** linha (P ou S) do grupo 06:

```r
chaves_apac_med <- pa3 %>%
  filter(DOCORIG %in% c("P", "S"), substr(PA_PROC_ID, 1, 2) == "06") %>%
  distinct(PA_AUTORIZ, PA_CMP, Ano)
```

- Depois disso, conferir se sobra alguma APAC em `atend`. Se sobrar quase nada, a análise de "asma como diagnóstico secundário (APAC)" perde objeto e deve sair do texto.

---

## 🔴 3. "Urgência" é efeito do tipo de estabelecimento

**Evidência** (BPA-I por tipo de unidade, `PA_TPUPS`):

| Tipo | Atendimentos | % urgência |
|---|---|---|
| 36 Clínica/centro de especialidade | 16.564 | 0,2% |
| 73 Pronto-atendimento | 14.597 | 95,9% |
| 05 Hospital geral | 3.886 | 49,1% |
| 15 Unidade mista | 391 | 95,7% |

Quatro estabelecimentos concentram 62% dos BPA-I. Em 2024, um único estabelecimento responde por 61% do ano. O número de unidades que notificam por ano varia de 18 a 32.

**Impacto.**
- A **tendência de urgência** (1% em 2015 → 38% em 2018 → 72% em 2022 → 31% em 2024 → 62% em 2025) acompanha quais unidades notificaram em cada ano. Não indica piora ou melhora da asma.
- **Maceió × interior** (49,8% × 34,7% de urgência, V = 0,15) é, provavelmente, diferença de rede de serviços.
- A **correlação ecológica** (seção 10) e o "teve urgência" do vínculo BI × AM herdam o mesmo problema.

**Ajuste.**
- Criar `Tipo_Servico` (pronto-atendimento/hospital × ambulatório especializado × APS) a partir de `PA_TPUPS` e estratificar todos os indicadores de urgência por ele.
- Para a tendência: restringir a estabelecimentos que notificaram em todos os anos (painel estável) ou modelar com efeito fixo de estabelecimento, por exemplo `glm(Urgencia ~ factor(Ano) + PA_CODUNI, family = binomial)`.
- Fazer uma sensibilidade que retira o maior estabelecimento de cada ano.
- No texto, tratar "caráter de urgência" como **atendimento em serviço de urgência**, não como gravidade.

---

## 🔴 4. Pseudorreplicação: atendimentos não são independentes

**Problema.** Os qui-quadrados (`testes`), os IC de Wilson (`indicadores`) e as comparações Maceió × interior usam **atendimento** como unidade. O BI mostra 40,6% de retorno, e em 2024 a média chega a 4,4 atendimentos por paciente (sessões de fisioterapia).

**Impacto.** Erros-padrão subestimados, IC estreitos demais e p-valores pequenos demais (p = 1e-227 para raça/cor). O V de Cramér ajuda, mas também pondera pacientes frequentes.

**Ajuste.**
- Fazer as **comparações inferenciais no nível do paciente** (BI: 1 linha por CNS, variáveis do 1º atendimento ou "algum atendimento com X"). Isso já existe em `pacientes`; basta mover os testes para lá.
- Para manter atendimento como unidade, usar **IC e testes robustos a cluster por CNS**: `survey::svydesign(ids = ~CNS_PAC, data = bi_atend)` com `svyciprop`/`svychisq`, ou GEE (`geepack::geeglm(..., id = CNS_PAC, corstr = "exchangeable")`).
- No PA não há identificador, então declarar essa limitação e reportar os resultados de atendimento só como **descritivos**, sem p-valor.

---

## 🟠 5. "Etapa de tratamento" pela apresentação, não pela dose

**Problema.** A etapa 2 × 3 é definida pelo texto "BUDESONIDA 400" (`etapa_num`). No PCDT, a cápsula 12/400 serve a várias etapas, e a etapa depende da **dose diária**: número de inalações e de cápsulas por mês. A 6/200 é basicamente a apresentação pediátrica e de dose baixa.

**Evidência.** As 1.127 "reduções" 400 → 200 e a associação forte com a idade (V = 0,20; 33% das crianças de 5–9 anos na "etapa 2") são compatíveis com a lógica de **apresentação por idade**, não com degrau terapêutico. Há 5 crianças de 0–4 anos com 12/400, abaixo da idade mínima do PCDT.

**Ajuste.**
- Estimar a **dose diária** pela quantidade dispensada no mês: `PA_QTDAPR` da linha do medicamento, pareada por `AP_AUTORIZ × AP_CMP`, ou o campo de quantidade do próprio AM, se for lido. Por exemplo, cápsulas/mês ÷ 30 × µg de budesonida → classificar em dose baixa, média ou alta de corticoide inalatório (GINA).
- Renomear a variável atual para **"apresentação dispensada"** e retirar a palavra "etapa" das conclusões até ter a dose.
- Rotular "Reduziu" e "Escalonou" na 9e como **troca de apresentação**.

---

## 🟠 6. APAC "inicial" ≠ início do tratamento

**Evidência.** `AP_TPAPAC = 1` aparece em 34% dos registros: a APAC vale 3 competências e é reemitida como "inicial" a cada renovação. Em todo ano, `APAC_Iniciais` ≈ pacientes em tratamento (ex.: 2.595 de 2.701 em 2015).

**Impacto.** A decisão (j) do cabeçalho e a coluna `APAC_Iniciais` estão incorretas como "início de tratamento"/incidência.

**Ajuste.** Definir **paciente incidente** como o 1º mês com APAC após um **washout** sem dispensação (ex.: ≥ 12 meses sem APAC; exige começar a contar em 2016). Corrigir o texto do cabeçalho e o rótulo da coluna.

---

## 🟠 7. "Atendimento" é uma unidade heterogênea

**Evidência.** Os procedimentos mais frequentes do BPA-I são 0301060096 (26%, atendimento em urgência), 0302040021 e 0302040013 (42% somados, **fisioterapia respiratória**; CBO 2236 = fisioterapeuta, 15.707 registros) e 0301010072 (consulta especializada, 10%).

**Impacto.** Sessões de fisioterapia (várias por paciente e por mês) inflam a contagem de atendimentos, o "nº de atendimentos por paciente" e a tendência de 2024. Também diluem o % de urgência.

**Ajuste.**
- Criar `Tipo_Atendimento` = consulta médica / urgência / fisioterapia / outros, a partir de `PA_PROC_ID` + CBO.
- Reportar os indicadores por tipo.
- Usar **contato-dia** (paciente × estabelecimento × mês) como unidade alternativa no BI.

---

## 🟠 8. Sem denominador populacional

**Problema.** `arquivo_pop <- NA`. Tudo é contagem ou proporção entre atendimentos (perfil de quem usa o serviço, não de quem tem asma).

**Ajuste.**
- Baixar as estimativas populacionais por município, sexo e faixa etária (DATASUS/IBGE, projeções 2015–2025) e calcular:
  - taxa de atendimentos e de **pacientes** por 10 mil habitantes, **padronizada por idade** (método direto, população Brasil 2010 ou AL 2022);
  - taxa de pacientes em tratamento no CEAF por 10 mil (cobertura).
- Na comparação Maceió × interior, usar razão de taxas com IC (Poisson ou binomial negativa com offset log(pop)).

---

## 🟠 9. Mudanças de cobertura do sistema ao longo do período

**Pontos não tratados no código:**
- A **APS deixou de registrar no SIA** (e-SUS AB/SISAB). Consultas de asma na atenção básica não estão nos dados, e a fração capturada mudou entre 2015 e 2018 (2015: 288 BPA-I; 2018: 3.345).
- O preenchimento do **CNS no BPA-I** começou baixo: só 117 pacientes em 2015.
- **COVID-19 (2020–2021):** queda de uso e atendimentos respiratórios reclassificados.
- 2025 ainda incompleto (apresentação tardia além de `meses_extra`).

**Ajuste.**
- Fazer a tendência só a partir do ano em que o volume se estabiliza (ex.: 2018+) e marcar 2020–2021 nos gráficos.
- Fazer uma sensibilidade sem 2020–2021.
- Testar tendência com **Prais-Winsten** ou **joinpoint** sobre taxas, não sobre contagens brutas.
- Declarar explicitamente: "os dados descrevem a asma atendida na média complexidade e na urgência registradas no SIA, não a carga de asma da população".

---

## 🟠 10. Comparações BI × AM e antes × depois

**Problemas:**
- **9d.** Comparar "recebe AM" × "não recebe" quanto à urgência confunde idade (mediana 49 × 13 anos), tipo de serviço (item 3) e tempo de exposição. Além disso, quem está no CEAF pode estar fora do BI simplesmente por ser acompanhado na APS.
- **9e(b).** Não há grupo controle. Há **regressão à média** (o CEAF começa após uma piora), tendência secular e mudança de notificação dos estabelecimentos (item 3). Só 341 de 7.949 pacientes (4%) entram, uma amostra selecionada.

**Ajuste.**
- **9d:** regressão logística no nível do paciente: `Teve_Urgencia ~ Em_Tratamento_AM + idade (spline) + sexo + Local + ano do 1º atendimento + tipo de serviço do 1º atendimento`, reportando a OR ajustada.
- **9e:** desenho **autocontrolado com janelas múltiplas** (−24 a −13, −12 a −1, +1 a +12, +13 a +24) para mostrar se a queda já vinha ocorrendo. Comparar com um grupo de pacientes do BI **sem AM**, pareados por idade, sexo, estabelecimento e mês índice (diferença-em-diferenças). Modelar a contagem com Poisson condicional por paciente.
- Usar o rótulo "associação temporal" e não "efeito".

---

## 🟡 Melhorias menores

**11. Gestantes no IMC.** O IMC de gestantes entra na classificação. Excluir `Gestante_Algum_Mes == TRUE` do IMC adulto.

**12. Censura à esquerda.**
- `Pacientes_Novos_no_Periodo` em 2015 = todos (117); em 2016–2017 ainda é prevalente.
- Em `pacientes_am`, `Meses_Com_APAC` e `Duracao` de quem começou antes de 2015 ficam truncados.
- Ajuste: aplicar washout (item 6) e/ou excluir o 1º ano dessas métricas.

**13. Censura à direita na duração.** Usar **Kaplan-Meier** para o tempo até a interrupção (evento = ≥ 3 meses sem dispensação; censura no fim dos dados válidos), em vez de mediana de meses dispensados. O mesmo vale para o desfecho "interrompeu até 6 meses".

**14. Definição de interrupção.** O episódio vai do 1º ao último mês. Lacunas internas longas (≥ 3 meses) deveriam abrir um **novo episódio** (retomada), conceito padrão em persistência (gap de permissibilidade).

**15. Testes múltiplos.** São mais de 15 qui-quadrados entre seções 6, 9c e 9d. Priorizar tamanho de efeito com IC (V de Cramér com bootstrap, diferenças de proporção com IC) e aplicar Holm se houver p-valores. O limiar do V de Cramér (0,1/0,3/0,5) também deveria considerar os graus de liberdade (Cohen: w ajustado por `min(dim)-1`).

**16. Correlação ecológica (seção 10).**
- Muitos municípios com `Pct_Fora_Municipio = 100%` (teto), 44 municípios, proporções com denominadores muito desiguais.
- Ajuste: regressão binomial ponderada (ou beta-binomial) com o nº de atendimentos como peso, mapa coroplético das taxas e cautela com falácia ecológica.

**17. Decodificações a confirmar no dicionário (.cnv do TAB):**
- `decod_sexo` trata "3" como feminino;
- `AP_COIDADE` 2/3 → 0 ano (24 registros com 3);
- `decod_carater` 03–06 = acidentes;
- documentar a fonte de cada mapeamento.

**18. Raça/cor sem informação.** 21% no AM e 13% nas APAC. Comparar perfil com e sem raça e evitar conclusões raciais sem análise de missing (ou imputação múltipla como sensibilidade).

**19. Valores (R$).** `AP_VL_AP = 0` de 2015 a 2022 (aquisição centralizada). Não comparar custos entre anos e retirar o valor do perfil ou explicar.

**20. Reprodutibilidade.**
- O cache é guardado só pelo nome do arquivo. Se `regex_asma` ou `cols_*` mudarem, o cache antigo é reutilizado. Incluir um hash dos parâmetros no nome da pasta de cache.
- `listar_arquivos` via FTP ignora arquivos divididos (`a/b/c`).
- Fixar as versões dos pacotes (`renv::snapshot()`) e gravar `sessionInfo()` no DOCX.
- Separar o script em etapas (extração → limpeza → análises → relatório) ou usar `targets`.
- O AMAL2605 está ausente no FTP: registrar isso no fluxograma.

---

## Oportunidades para fortalecer o trabalho

1. **Ligação com o SIH (internações por asma).** Mesmo sem CNS, é possível analisar **taxas de internação por asma (condição sensível à APS)** por município e correlacioná-las com a cobertura do CEAF e da urgência ambulatorial. Isso responde à pergunta original sobre desfecho hospitalar.
2. **Cascata do cuidado.** População estimada com asma (prevalência de ~10–20% pela PNS/ISAAC) → atendida no SIA → em tratamento no CEAF → persistente ≥ 6 meses. É uma figura muito informativa para o IC.
3. **Interrupção da oferta.** As quedas de dispensação de 2021 (abr–jul) e 2025 podem ser desabastecimento. Vale confirmar com a SESAU-AL e, se confirmado, analisar como evento: houve aumento de urgência nos meses seguintes?
4. **Geografia.** Taxas por região de saúde de AL (10 regiões) em vez de Maceió × interior, com mapa.
5. **Checklist RECORD** (extensão do STROBE para dados administrativos rotineiros) para estruturar métodos e limitações do relatório.

---

## Ordem sugerida de execução

1. Itens 2 e 1 (correções de seleção): mudam os números de base.
2. Itens 3 e 7 (tipo de serviço/atendimento) e 6 (incidência): redefinem variáveis.
3. Item 4 (análises no nível do paciente / robustas a cluster).
4. Itens 8 e 9 (população, taxas, tendência com painel estável).
5. Itens 5 e 10 (dose e desenhos comparativos): exigem mais modelagem.
6. Melhorias 11–20.
