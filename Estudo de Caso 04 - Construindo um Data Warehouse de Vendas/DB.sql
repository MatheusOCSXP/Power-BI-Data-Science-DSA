PGDMP             	            z            vendas    14.2    14.2                0    0    ENCODING    ENCODING        SET client_encoding = 'UTF8';
                      false            	           0    0 
   STDSTRINGS 
   STDSTRINGS     (   SET standard_conforming_strings = 'on';
                      false            
           0    0 
   SEARCHPATH 
   SEARCHPATH     8   SELECT pg_catalog.set_config('search_path', '', false);
                      false                       1262    16394    vendas    DATABASE     f   CREATE DATABASE vendas WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'Portuguese_Brazil.1252';
    DROP DATABASE vendas;
                postgres    false                       0    0    DATABASE vendas    COMMENT     E   COMMENT ON DATABASE vendas IS 'Banco de dados do sistema de vendas';
                   postgres    false    3339                        2615    16395    mkt    SCHEMA        CREATE SCHEMA mkt;
    DROP SCHEMA mkt;
                postgres    false                       0    0 
   SCHEMA mkt    COMMENT     @   COMMENT ON SCHEMA mkt IS 'Esquema de acesso ao banco de dados';
                   postgres    false    5            �            1259    16404    TB_DATA    TABLE     �   CREATE TABLE mkt."TB_DATA" (
    "DATA_COMPLETA" character varying(20) NOT NULL COLLATE pg_catalog."pt-BR-x-icu",
    "DIA" integer,
    "MES" integer,
    "ANO" integer
);
    DROP TABLE mkt."TB_DATA";
       mkt         heap    postgres    false    5            �            1259    16399    TB_LOJA    TABLE     �   CREATE TABLE mkt."TB_LOJA" (
    "ID_LOJA" character varying(20) NOT NULL,
    "CIDADE" character varying(20),
    "ESTADO" character varying(20)
);
    DROP TABLE mkt."TB_LOJA";
       mkt         heap    postgres    false    5            �            1259    16409 
   TB_PRODUTO    TABLE     �   CREATE TABLE mkt."TB_PRODUTO" (
    "ID_PRODUTO" character varying(20) NOT NULL,
    "NOME_PRODUTO" character varying(50),
    "CATEGORIA" character varying(20),
    "SEGMENTO" character varying(20),
    "MARCA" character varying(20)
);
    DROP TABLE mkt."TB_PRODUTO";
       mkt         heap    postgres    false    5            �            1259    16419 	   TB_VENDAS    TABLE     +  CREATE TABLE mkt."TB_VENDAS" (
    "ID_PRODUTO" character varying(20) NOT NULL,
    "ID_LOJA" character varying(20) NOT NULL,
    "ID_VENDEDOR" character varying(20) NOT NULL,
    "DATA_COMPLETA" character varying(20) NOT NULL COLLATE pg_catalog."pt-BR-x-icu",
    "VALOR_VENDA" double precision
);
    DROP TABLE mkt."TB_VENDAS";
       mkt         heap    postgres    false    5            �            1259    16414    TB_VENDEDOR    TABLE     �   CREATE TABLE mkt."TB_VENDEDOR" (
    "ID_VENDEDOR" character varying(20) NOT NULL,
    "NOME" character varying(20),
    "SOBRENOME" character varying(20)
);
    DROP TABLE mkt."TB_VENDEDOR";
       mkt         heap    postgres    false    5                      0    16404    TB_DATA 
   TABLE DATA           F   COPY mkt."TB_DATA" ("DATA_COMPLETA", "DIA", "MES", "ANO") FROM stdin;
    mkt          postgres    false    211   H                 0    16399    TB_LOJA 
   TABLE DATA           ?   COPY mkt."TB_LOJA" ("ID_LOJA", "CIDADE", "ESTADO") FROM stdin;
    mkt          postgres    false    210   �                 0    16409 
   TB_PRODUTO 
   TABLE DATA           c   COPY mkt."TB_PRODUTO" ("ID_PRODUTO", "NOME_PRODUTO", "CATEGORIA", "SEGMENTO", "MARCA") FROM stdin;
    mkt          postgres    false    212                    0    16419 	   TB_VENDAS 
   TABLE DATA           j   COPY mkt."TB_VENDAS" ("ID_PRODUTO", "ID_LOJA", "ID_VENDEDOR", "DATA_COMPLETA", "VALOR_VENDA") FROM stdin;
    mkt          postgres    false    214   �                  0    16414    TB_VENDEDOR 
   TABLE DATA           H   COPY mkt."TB_VENDEDOR" ("ID_VENDEDOR", "NOME", "SOBRENOME") FROM stdin;
    mkt          postgres    false    213   �-       o           2606    16453    TB_DATA TB_DATA_pkey 
   CONSTRAINT     `   ALTER TABLE ONLY mkt."TB_DATA"
    ADD CONSTRAINT "TB_DATA_pkey" PRIMARY KEY ("DATA_COMPLETA");
 ?   ALTER TABLE ONLY mkt."TB_DATA" DROP CONSTRAINT "TB_DATA_pkey";
       mkt            postgres    false    211            m           2606    16425    TB_LOJA TB_LOJA_pkey 
   CONSTRAINT     Z   ALTER TABLE ONLY mkt."TB_LOJA"
    ADD CONSTRAINT "TB_LOJA_pkey" PRIMARY KEY ("ID_LOJA");
 ?   ALTER TABLE ONLY mkt."TB_LOJA" DROP CONSTRAINT "TB_LOJA_pkey";
       mkt            postgres    false    210            q           2606    16459    TB_PRODUTO TB_PRODUTO_pkey 
   CONSTRAINT     c   ALTER TABLE ONLY mkt."TB_PRODUTO"
    ADD CONSTRAINT "TB_PRODUTO_pkey" PRIMARY KEY ("ID_PRODUTO");
 E   ALTER TABLE ONLY mkt."TB_PRODUTO" DROP CONSTRAINT "TB_PRODUTO_pkey";
       mkt            postgres    false    212            u           2606    16502    TB_VENDAS TB_VENDAS_pkey 
   CONSTRAINT     �   ALTER TABLE ONLY mkt."TB_VENDAS"
    ADD CONSTRAINT "TB_VENDAS_pkey" PRIMARY KEY ("ID_PRODUTO", "ID_LOJA", "ID_VENDEDOR", "DATA_COMPLETA");
 C   ALTER TABLE ONLY mkt."TB_VENDAS" DROP CONSTRAINT "TB_VENDAS_pkey";
       mkt            postgres    false    214    214    214    214            s           2606    16439    TB_VENDEDOR TB_VENDEDOR_pkey 
   CONSTRAINT     f   ALTER TABLE ONLY mkt."TB_VENDEDOR"
    ADD CONSTRAINT "TB_VENDEDOR_pkey" PRIMARY KEY ("ID_VENDEDOR");
 G   ALTER TABLE ONLY mkt."TB_VENDEDOR" DROP CONSTRAINT "TB_VENDEDOR_pkey";
       mkt            postgres    false    213               8  x�m�ۍ� ���u1���i"��:�cD�_��S�8��9s����~��8��,�q6Ka��rǭ�U�v�u7ٛ�ٛ�g��m��2X�`��nwF���9�	r�&�M�� 7An�܄��,Yv�r\�&�W��X+�U����������E�ULޡ��C�x�bkе����v{�[�=�9�w��
Xk`Xw�������+0�f	,��
��.��vg��!�C�� 7An���	r�&�M�?�R�e�,��XM�� ߹7V�`�{0��jͻ1��j�m����Ы�7V�ޡ�7V�`���{�O�         �   x���=N1���)|��$��2�PP$�K�hF�#��vRp�(5U�������{��gt���|f����[)VJ��ֱ��@o쓅����(L�ϯȷU��˽���OZ�k��v��%�]�{��ҠO,��m*����G�@c@�O¬�27莸� =�~����1�����Ǽ���u(\S��O��}j9N��n�|�iA��e`G��<�!> �^�F         c  x����r�6���S��!p/@`��9�$�g�f�膕ؔ-���:������
5�DT�8��$�;#�`�\���A��/�8�v��S[=����œ��N�j���{���f��ӟ�v��.�N.>E4|D�#��#��0�G(���~S��#x/��Bx/��Bx/��|��N��4�Z~-�f��P���ψQ���ψQ��P~S]~��k�q�������3�����rz~9=��;�3ϗ_ϟ��{x/�E����"�^ދ�W��W���i;U�P~�Q��B�AI�)G�E�>#�O9�O9�x/�)G�)G�)G���J�H_\���7{AJ��H�-������z�(5#(�Y@�XY@i��J��J}�J��A�ԤR�HMn�ԥ�!(�Y �ׅÀ⚐�gA���]DC/"{�,��=L�,�b��X��0`=(~n���Y �A�0��?��؊T��2 ML����v5&ޘ�ś�yQ� Wpl�Lݑ{�,���۰�!���!Rw�^C�d�eX�m��G��L�#��ږ�x��=%��a��<o��f\��=�/^��i|��z�n��f�t���*$p�!RxSs��p�������9����xd���FAα�9d�3t�Wr8AO'W���z�E��P�_�W�T��?u,2��؀HH�6����&
i�Ms4w�l��{���Q�,D	D)D�5�Z��Z��XkFʌ-�'Y)&3���glA��؂2cȌ-�3�`t�؂�1c	���J	��JJ�'�{%%��Ž��+I��^I�{%��+)��d,�U �
�W��*^«@x�"�U$���UW"�8E"�8E"�8E"�8E"�8E"�O"�Ok�8��dê_��:�Hh/7��7{���=�����Ȧ����m������3�N���*1�O����Ô~�׺o�XK�J��`�F�55��fg��{dvR�,���v��a�`q�LMxe��q~M$��8�&���jR�@8����� ' � �@΃\r�/�}1�/�ŀ���b@_��k�U�W[��U��Z���uxﲎX+��5����g�G�p�1���5�nx|�����^y�+OxՄ��n�퇯�_ֹ&b\�A΀�999r��}	�/B�Q��ћ���G��&D_��	�ׄ�kB�5!��}M��&D_��	�ׄ�kB�5�W�Y�O��lvt��ӡ�L�k��u�k?,�j�3��yr�z�3K�����L9:S��)�ϔ3��Z�������^����W�/��l�Pc����#XO��
vWP��
*�]A�+�`w쮠��T���
vWP��
&���WS�+�7�>�6o���և����|��T�t0�a��� �%m�&a�,���Ÿ~_=��ꦾ�~<]<��Ma���>S��l�Ӹ��z�~���ql��)}�at��-5(j}��O�6�z.��.��.R� n��?���!\t�cpU�C���ac��][�ؽ�
C%ݙ`��(\x���C���� F�}K���X�&��(����6	��DpQp�I`�բ���C-z�P�^:��Y�!�O�*\@���A|F�L�xyV�0u����|�����G�.���7���i��v�v?��۴��P����K���e6lXX�1�g���Gv�bwb��U��2�Nt���Π�EAAAEA��(AУ�x����Qs��>��|���l����="��c&{|�1�x���p����؀�0�~���ڛ�U�����FSm�Q���?k�Q�oNC���4GGGq�����Vɒc�,9Vɒc���ցu��oNNN�`��Q         �  x���I�%�D�YwQVp�i��j#�!�t�s(��!j������N��?���]��苮k|���ޙ�.�M�o���j����`uc����`]0����>X`��������`�|�������Cj�WXȂdy�2��сlv�7Y���7J����׾�c\�M��v?7�''웮��k<<
��7�*{y4E�Z`$~Û#�<�� ^�Y^9s�4U�+�ym�7�G˯�|������1��;#�%�؉���ܣ�i����8l��-�N9�?X��Ӱ�1���H�O?�(��"<��MV���M=���eFI�$7i�eك�~�/Q�痏:�h�hy�_ �<̯�RA����/(}�������H*(~���������*��~��_��XŴ���C���g�)��d�R�&�2��B;7�ɝL��C���D';��Jt�C���D'A���L����S�}L+q����̾㿟�`�{�)��LQ�ߥe��A�H����i���� ��l�٬�{b}��X$g��Gxln�N�1n*��`�������m'#�$�T�u�����A�m��ᄫ��O��X	7�#k���O'�7�fg}\�Q阸6�ƻ�!m�+����
��)�	FӸ���6~5-ڦ�y��e�*�_*,�*[�v�wĜnk�^5�s*���T�\s��?Tw"����}f����S�j��M.�XL�=��>��R%k�_���/�/2��8r�/]���-�yx��������=�q=�;�-Q+�6hjFۯ-3?[8qu��*��DШ*qCL}(���	�b��+S�� �~Z)�`��׶e5��3�_�����TΤ7�U�0
ܣ!�oI��&��۩0T��������f�5`��t�؈Zq�MN�*��'��ڶ�C�hj0[X��6D�������	1Y�����V�m��hj'mA�����4�����λ���@�DV���WɊi���$�����Z�iM�S��DY��q	��2�@��9Q�,j���(G-���6�����n@�A�X��'IPc��Q	j,mU��ƵGìI�X�*�
	jP��!��> ��5|$�k{�:�&�cn&�V#tn@m@酹��Du, H�sJݘ(\Nel�Q9qe��T7���X�R�W��k�ד�Є�k���i Ƹ�� ��	��7F���0��x`��8�SN����38Z�
LL��5�>��j1s1C�/׍u�}�)���܎	�:>u�	"ki��bҪ.'F��4�㹨��n���!��﹧�6\I9��W�_�?մ�n��C�.(�n��;\�Ƹ��W�^k1��j��T�����0�/SuR������q�GƱo�n�g�Ⱗ��p�S⩦�u_bx��=�)&�L6VRl6��~[g��ⱖbN����BB)�=���爡�6��Dׯ�q��D�����im�|��0ř�kS�;����ί��y*��:��:��}���#>���p��1���xD=��ƐOi���� juquc����>�5e.���*�1�py��Ψ�dw��F���v�6��[!kܧ����5._N��M��?���c&�M<5E:?eu�)����o��aĢN�扸�Ѣ�U�8bY��1*`OY�|��S3*�^��bL�&�B�*��+�ǔaA0(`-\\Jn$��p*�f�-�`�oF�
t�)`�Y�������J�0�~U]���1ˡ�b��qhm���-ߎG՜G�K[�������hm;��
����/K�
˩}\X��SPgf�7=�)#��f�O=-�W+�x���:����,��8fm�+����`b�g�"�ʆ�of��cX^K?�A䧜.�_���J�M2��v0��w۝�u����N����k���
�&�����$�����3�v�{��_�vXX�+�ɻ/�#� l�e���L���6�����]�����JFS����kppdD5XX�0�������j�������a�Q&��(S�)��7N�
5���V����ڂ��a�T5�Zl��J����S�0
b�$l�B:hjG����*LM?�mBT]�t��X�	ѢV����p���# ��)`����nSf�]H�.nܧ\��-l�6��ZNEU�z�g�/��X�ۧ�#,?�s�|*���9 4 &}l=j\H��z�}V~*�F��^L���|������5+�\w�z��fT�>�r���-�`��;;fCib03Z��n�����o�8�:����䂏����F���_��&����Q�fJp��{��TC)�"������g&W�˱��E#W�3���p�m4_3
����F�F�y�a����;����N�a�ֶ[9S��+�@N�h�`_��}7��2�����P<��
����"�h��ޅ//h*��S��6_01K;)��@�B-��
��u���-8�<VR��VK��\�)�)�K{?X>D=����ˉ�����Y?=LR,��)6���������L_ �8%��,!<���45����F���*��^A�b%5�5��Eg
v���9<%	U�ʹ�E~���e/�=U3J<�2J=�3�xjdTu{ܹ�P�(�L��P�H�L��P�\�f��T�g�4/5�LC��R3U�]KA?C��mB�5é�@ͨ�����F$����������q����[v��r���\�n<i�p��4��ݟ��V��p�5���/U_<i���IK���3j!�g/xj�O�7��ԥÿ�)�IK��_�����'~��bz9��Oҋ�����U*i�Ko�F%���Qͨ��u�?����'c���(�6�2J<E�/��T�'�R!�dg?������ُ�)�.P�p�yw���z�,��z���;�fz�,	�zA��o�M��t���Or�^�S�Q�{-Il�>�JZ
~�Ml�~7��Ħ$oJJ�
I>�d�M׿�N^�TH����T�%z?:S
Qg���O��T�H�p��gX8{��
C9�}�ȧ��QG��E���G���Q;�ϫ��S�+����Wp�ѭ>o�:bq�rywl?���#����>!��ȷ�f���f�6���7[�Ც[��ؐj�ڧ��w�X���bߙ�x����I��񌃩ɶ�xv�����܂�g�)��[�Y�<�M�ٌV�l�z�j�\�{6�u��7̈`zψ���=̨ �g�1�����Ũ �g7�L����@o�g-�� �g7�Q���o�߿~���i,�         �   x�=�K
�0�דS���v�M� �J܌d��:�iR��x�^�Ԫۏ���m�{"~+�¹%�B�؂=r�}�j��2���t�|�Ƥ`렿��+��
oI�]AE*(>�3ޯ���f����E�S�^��8���a��b$5��1��k:x     