PGDMP             	            z            RH    14.2    14.2     ?           0    0    ENCODING    ENCODING        SET client_encoding = 'UTF8';
                      false            ?           0    0 
   STDSTRINGS 
   STDSTRINGS     (   SET standard_conforming_strings = 'on';
                      false            ?           0    0 
   SEARCHPATH 
   SEARCHPATH     8   SELECT pg_catalog.set_config('search_path', '', false);
                      false            ?           1262    16507    RH    DATABASE     d   CREATE DATABASE "RH" WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'Portuguese_Brazil.1252';
    DROP DATABASE "RH";
                postgres    false                        2615    16508    cap16    SCHEMA        CREATE SCHEMA cap16;
    DROP SCHEMA cap16;
                postgres    false            ?            1259    16518    TB_ENDERECO    TABLE       CREATE TABLE cap16."TB_ENDERECO" (
    id_end integer NOT NULL,
    rua character varying(30),
    numero character varying(30),
    bairro character varying(30),
    cep character varying(10),
    estado character varying(30),
    pais character varying(30),
    id_func integer
);
     DROP TABLE cap16."TB_ENDERECO";
       cap16         heap    postgres    false    4            ?            1259    16509    TB_FUNC    TABLE       CREATE TABLE cap16."TB_FUNC" (
    id integer NOT NULL,
    estado_civil character varying(30),
    grau_instrucao character varying(30),
    numero_filhos character varying(2),
    salario_hora double precision,
    idade integer,
    reg_procedencia character varying(30)
);
    DROP TABLE cap16."TB_FUNC";
       cap16         heap    postgres    false    4            ?            1259    16523    VW_FUNC    VIEW     ?   CREATE VIEW cap16."VW_FUNC" AS
 SELECT round(avg("TB_FUNC".salario_hora)) AS round,
    "TB_ENDERECO".estado
   FROM (cap16."TB_FUNC"
     JOIN cap16."TB_ENDERECO" ON (("TB_FUNC".id = "TB_ENDERECO".id_func)))
  GROUP BY "TB_ENDERECO".estado;
    DROP VIEW cap16."VW_FUNC";
       cap16          postgres    false    210    211    211    210    4            ?          0    16518    TB_ENDERECO 
   TABLE DATA           _   COPY cap16."TB_ENDERECO" (id_end, rua, numero, bairro, cep, estado, pais, id_func) FROM stdin;
    cap16          postgres    false    211   \       ?          0    16509    TB_FUNC 
   TABLE DATA           y   COPY cap16."TB_FUNC" (id, estado_civil, grau_instrucao, numero_filhos, salario_hora, idade, reg_procedencia) FROM stdin;
    cap16          postgres    false    210          g           2606    16522    TB_ENDERECO TB_ENDERECO_pkey 
   CONSTRAINT     a   ALTER TABLE ONLY cap16."TB_ENDERECO"
    ADD CONSTRAINT "TB_ENDERECO_pkey" PRIMARY KEY (id_end);
 I   ALTER TABLE ONLY cap16."TB_ENDERECO" DROP CONSTRAINT "TB_ENDERECO_pkey";
       cap16            postgres    false    211            e           2606    16513    TB_FUNC TB_FUNC_pkey 
   CONSTRAINT     U   ALTER TABLE ONLY cap16."TB_FUNC"
    ADD CONSTRAINT "TB_FUNC_pkey" PRIMARY KEY (id);
 A   ALTER TABLE ONLY cap16."TB_FUNC" DROP CONSTRAINT "TB_FUNC_pkey";
       cap16            postgres    false    210            ?   ?   x?}ͻ
?@??z?)???%&?FPn#??͐?"?5?Oo"+?i?ǯ5?tI?!\?c??3?o<"??P?\??8? ?P?O0J#,?????????^?M??ׇ?(G?yѻY[h??/i??$??X?$?5K???2[ZϮ?НH8?+笯W\?n[???1O      ?     x?}?K??0D??S?	Zt???.?A#!%???8Qc?Y?S???-?????ӈn??a??]???w??????<l???ѧ??47m?8???????{H?~X??'??k?1??"?~d????&?L???M|t???L?aW???N2?p?4Rа;?/??TÆ??RN?`?*!n?'Ӽ>^~?E
?7?E?J????E?C????ߊ?$[9l?Gh?)????h`??Q??wc?J*g??5J?6?)8PJ??FO+?day}AN??????????9?????	     