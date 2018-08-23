# prolog_RC

%negacao
not(B) :- call(B), !, fail.

%Quid_Iuris
%se facto provado = contrato/obrigação --> ir buscar responsabilidade contratual (RCC)
%se resto --> ir buscar responsabilidade extracontratual (RCE)

%artigo483 - principio geral da resp. civil extratracontratual
responsabilidade_Civil_Ereu(X)t(RCE). 

RCE --> obrigacao_de_indemnizar(reu(X),autor(Y)).

obrigacao_de_indemnizar(reu(X),autor(Y)) --> (ato_voluntario(reu(X)); omissao(reu(X))),
					     (dolo(reu(X)) ; culpa(reu(X)); responsabilidade_objetiva(reu(X))),
					     (viola_direito(reu(X),autor(Y)), viola_interesse_protegido(reu(X),autor(Y))), %definir_interesse_protegido
					     causa_dano(reu(X),autor(Y)),
					     nexo_de_causalidade((ato_voluntario(reu(X)); omissao(reu(X))),causa_dano(reu(X),autor(Y))). 
					      

%como decidir quando ha culpa do lesado?

%definicao atoVoluntarios
ato_voluntario(reu(X)) --> ato(reu(X)), 
			   ser_humano(reu(X)),
			   not(inimputavel(reu(X))).

ser_humano(reu(X)):- homem(reu(X));
		     mulher(reu(X)).

%definicao omissao
omissao(reu(X)):- not(ato(reu(X)), 
		  ser_humano(reu(X)), 
		  not(inimputavel(reu(X))),
		  dever(ato_voluntario(reu(X))).

%definicao dolo
dolo(reu(X)) :- dolo_direto(reu(X));
		dolo_necessario(reu(X));
		dolo_eventual(reu(X)).

dolo_direto(reu(X)) --> intencao((ato_voluntario(reu(X)); omissao(reu(X))), ato_ilicito).

dolo_necessario(reu(X)) --> intencao((ato_voluntario(reu(X)); omissao(reu(X))), ato_licito),
			    consciencia_consequencia(ato_ilicito).

dolo_eventual(reu(X)) --> intencao((ato_voluntario(reu(X)); omissao(reu(X))), ato_licito),
			  aceita_probabilidade(ato_ilicito).

%definicao de culpa
culpa(reu(X)) --> not(bom_pai_de_familia(reu(X))), not(inimputavel(reu(X))).
culpa(reu(X)) :- negligente(reu(X)).

%definicao viola_direito + causas de exclusao da ilicitude
viola_direito(reu(X),autor(Y)) --> direito_privado(reu(X)),
				   viola_direito(reu(X),autor(Y)),
				   not(causa_de_exclusao(ato_voluntario(reu(X)); omissao(reu(X)))).

%definicao de viola_interesse_protegido?

causa_de_exclusao :- direito_privado(reu(X)),
		     legitima_defesa(ato_voluntario(reu(X)); omissao(reu(X)));
	       	     estado_de_necessidade(ato_voluntario(reu(X)); omissao(reu(X)));
		     acao_direta(ato_voluntario(reu(X)); omissao(reu(X)));
		     consentimento_do_lesado(autor(Y));
		     colisao_de_direitos(reu(X),autor(Y));
		     dever_obd_hierarq(reu(X),superior_hierarquico).

legitima_defesa --> direito_privado(reu(X))
		    viola_direito(autor(Y),reu(X)),
	            defende(reu(X),direito_privado(reu(X))),
		    indispensavel(ato_voluntario(reu(X)); omissao(reu(X))),
		    perigo_atual(autor(Y)),
		    proporcionalidade(ato_voluntario(reu(X)); omissao(reu(X))).

%nota: esta definicao permite a legitima defesa de legitima defesa?
%ver definicao na prata

estado_de_necessidade --> direito_privado(reu(X)),
			  defende(reu(X),direito_privado(reu(X))),
		          danifica(reu(X),coisa(C),
			  pertence(coisa(C),autor(Y)),
		          indispensavel(ato_voluntario(reu(X)); omissao(reu(X))),
			  perigo_atual(P),
			  proporcionalidade(ato_voluntario(reu(X)); omissao(reu(X))).
%ver definicao na prata

%C = qualquer objeto/coisa

%por definiir
%acao_direta :- recurso_a_forca(reu(X)),
		sem_meios_coercivos_normais(defende(reu(X),direito_privado(reu(X)))
		indispensavel(ato_voluntario(reu(X)); omissao(reu(X))),
		perigo_atual(P),
		proporcionalidade(ato_voluntario(reu(X)); omissao(reu(X))).

%falta definicao de acao direta por erro.

recurso_a_forca --> apropriacao(reu(X),coisa(C)); 
		    destruicao(reu(X),coisa(C));
		    deteriorizacao(reu(X),coisa(C));
		    eliminacao(viola_direito(autor(Y),reu(X))).

indispensavel(ato_voluntario(reu(X)), omissao(reu(X))) --> sem_outro_meio(defende(reu(X),direito_privado(reu(X)))),
												sem_meios_coercivos_normais(defende(reu(X),direito_privado(reu(X)))). 
%definir meios
%ver def. indispensavel prata

proporcionalidade(ato_voluntario(reu(X)); omissao(reu(X))) :- valor(direito_privado(reu(X))) > valor(direito_privado(autor(Y))).
%ver definicao da prata

perigo_atual(autor(Y)) :- perigo(autor(Y)),
			  viola_direito(autor(Y),reu(X)),
			  atual(ato_voluntario(autor(Y)); omissao(autor(Y))).
			  %ver definicao prata

perigo_atual(P) --> perigo(P),
		    atual(P).

perigo(P) --> 

consentimento_do_lesado(autor(Y)) :- aceita(autor(Y),viola_direito(reu(X),autor(Y))).

colisao_de_direitos(reu(X),autor(Y)) --> direito_privado(reu(X)),
					 direito_privado(autor(Y)).
					 viola_direito(reu(X),autor(Y)),
					 viola_direito(autor(Y),reu(X)).
					 %ver definicao na prata
				

dever_obd_hierarq(reu(X),superior_hierarquico) --> direito_privado(autor(Y)),
						   viola_direito(reu(X),autor(Y)),
						   cumpre_ordem(reu(X),superior_hierarquico),												   (reclama(reu(X),superior_hierarquico);ordem_escrita(superior_hierarquico)).
%ver definicao na prata

%definicao de responsabilidade_objetiva
responsabilidade_objetiva(reu(X)) --> causa_dano(autor(Y)), comitente(reu(X),autor(Y)), comissario(autor(Y),reu(X)), %por_artigo_500;
				      causa_dano(animal(A)), pertence(A,reu(X));
				      causa_dano(veiculo), pertence(veiculo,reu(X)), %por_artigo_508
				      produtor(reu(X)), produto(C), por_circulação(reu(X),C).
				      transportador_aereo(reu(X)), (acidente(reu(X)); atraso(reu(X)); (destroi(reu(X),C), pertence(C,autor(Y))), causa_dano((acidente(reu(X));atraso(reu(X));destroi(reu(X),C)),autor(Y)); . %ver
				     (transporta(reu(X),bem_radioativo); possui(reu(X),bem_radioativo)), causa_dano(bem_radioativo,autor(Y)).
				     agencia_viagens(reu(X)), cliente(autor(Y),reu(X)), em_viagem(autor(Y)), causa_dano(P,autor(Y)). %ver
				     entidade(reu(X)), causa_dano(reu(X),ambiente), causa_dano(ambiente,autor(Y)).

%definicao causa_dano + patrimonio
causa_dano(reu(X),autor(Y)) :- danos_patrimoniais(reu(X),autor(Y));
			       danos_morais(reu(X),autor(Y)).


danos_patrimoniais(reu(X),autor(Y)) :- diminucao_patrimonio(autor(Y)).

diminuicao_Patrimonio(autor(Y)) :- dano_emergente(autor(Y));
				   lucro_cessante(autor(Y)).

dano_emergente(autor(Y)) :- atual(dano_emergente(autor(Y))), 
			   (valor_patrimonio = a, valor_patrimonio+dano_emergente = a - 1)
					

lucro_cessante(autor(Y)) :- futuro(lucro_cessante(autor(Y))),
		           (valor_patrimonio = a, valor_patrimonio+dano_emergente = a - 1)


danos_morais(reu(X),autor(Y),) :- mata(reu(X),autor(Y));
		 		  fere(reu(X),autor(Y)).
%mais exemplos?

%definicao nexo de causalidade
nexo_de_causalidade((ato_voluntario(reu(X)); omissao(reu(X))),causa_dano(reu(X),autor(Y))) --> condicao_sine_qua_non(ato_voluntario(reu(X)), causa_dano(reu(X),autor(Y))),
															causalidade_adequada(ato_voluntario(reu(X)), causa_dano(reu(X),autor(Y))).

%lista taxativa de direitos privados existentes - CC
@direitoPrivado(Z) <=> @integridadeFisica;
			@propriedade;
			@bomNome;


%artigo 487 - culpa e bom pai de bom_pai_de_familia
prova_culpa(autor(Y)).

culpa(reu(X)) --> not(bom_pai_de_familia(reu(X))), not(inimputavel(reu(X))).
culpa(reu(X)) :- negligente(reu(X)).

%fazer distincao entre culpa consciente e culpa inconsciente?

negligente(reu(X)) :- (preve(reu(X),ato_ilicito), omissao(reu(X)));
		      (não_preve(reu(X), ato_ilicito), previsivel(ato_ilicito)).

@bom_pai_de_familia(reu(X))/ <=> @razoavel(reu(X));
			@negligente(reu(X));
			@cuidadoso(reu(X)));
			@prudente(reu(X)));
			@cauteloso(reu(X)));
			@atento(reu(X)));
			@homem_medio(reu(X)).


%artigo 488 - inimputabilidade
ato_voluntario(reu(X)) --> ato(reu(X)), 
			   ser_humano(reu(X)),
			   not(inimputavel(reu(X))).
					
ser_humano(reu(X)) :- homem(reu(X));
		      mulher(reu(X)).

not(ato_voluntario(reu(X))) :- not(ato(reu(X)));
			       not(ser_humano(reu(X)));
			       not(imputavel(reu(X))).

@not(imputavel(reu(X)))/ <=> @inimputavel(reu(X));
		@incapacitado(reu(X)).

inimputavel(reu(X)) --> (bebado(reu(X)),not(culpa(reu(X)))).
inimputavel(reu(X)) --> (menor(reu(X)),not(culpa(reu(X)))).
inimputavel(reu(X)) --> (anomaliaPsiquica(reu(X)),not(culpa(reu(X)))).
inimputavel(reu(X)) --> (interdito(reu(X)),not(culpa(reu(X)))).
inimputavel(reu(X)) --> (inabilitado(reu(X)),not(culpa(reu(X)))).
incapacitado(reu(X)) --> (bebado(reu(X)),not(culpa(reu(X)))).
incapacitado(reu(X)) --> (drogado(reu(X)),not(culpa(reu(X)))).
incapacitado(reu(X)) --> (menor(reu(X)),not(culpa(reu(X)))).
incapacitado(reu(X)) --> (anomaliaPsiquica(reu(X)),not(culpa(reu(X)))).
incapacitado(reu(X)) --> (interdito(reu(X)),not(culpa(reu(X)))).
incapacitado(reu(X)) --> (inabilitado(reu(X)),not(culpa(reu(X)))).

menor(reu(X)) :- (idade(reu(X)) > 7).

%artigo 490 e 493 - dever de vigilância
viola_direito(reu(X),autor(Y)) :- obrigacao_de_vigiar(reu(X),Z),
				culpa(reu(X))
				causa_dano(Z,autor(Y)).

%por motivos de equidade o tribunal pode condenar um inimputavel a pagar indemnização se reu(X) 
%provar que cumpriu obrigação de vigiar, ou porque ninguém tinha obrigação de vigiar o inimputavel
%art. 489
%indemnizacao nao pode privar inimputavel 

%falta_definir_termos_de_responsabilidade_civil_contratual
responsabilidade_civil_contr(RCC). 

RCC --> obrigacao_de_indemnizar(reu(X),autor(Y)).

obrigacao_de_indemnizar(reu(X),autor(Y)) --> (contrato(Z); obrigacao(W)), 
					     devedor(reu(X),autor(Y)), 
					     incumprimento(reu(X),obrigacao(W)),
					     culpa(reu(X)). %ver 800 e 807

incumprimento(obrigacao(W)) --> (incumprimento_temporario(obrigacao(W)); incumprimento_definitivo(obrigacao(W))),
				(incumprimento_total(obrigacao(W)); incumprimento_parcial(obrigacao(W))).


%factos provados (ereu(X)emplificativo)
homem(dinis).
homem(lucas).
reu(dinis).
autor(lucas).
ato(dinis).
not(inimputavel(dinis)).
fere(dinis,lucas).
defende(dinis,integridadeFisica(dinis).
indispensavel(ato_voluntario(dinis)).
perigo_atual(lucas).
proporcionalidade(ato_voluntario(dinis)).
