angular.module('obrasPb').service('ObrasService', ['$http', '$q', function ($http, $q) {

    var self = this;
    const API_ENDPOINT = "/buildings";


    /**
     * Modelo de obra que será retornado enquanto
     * não colocamos o servidor para funcionar.
     */
    var obrasMock = [

        {
            cd_UGestora: 1,
            dt_Ano: 2010,
            nu_Obra: "00000000",
            dt_Cadastro: "2013-01-01 00:00:00",
            tp_Patrimonio: 'S',
            de_Localizacao: "Camara Municipal",
            de_Sucinta: "Ampliação do plenário",
            tp_Obra: 24,
            tp_CategoriaObra: 1,
            tp_Previsto: "102007",
            dt_Inicio: "2012-01-01 00:00:00",
            dt_Conclusao: "2013-01-01 00:00:00",
            dt_Recebimento: "2014-01-01 00:00:00",
            tp_FonteObra: 1,
            vl_Obra: 630.0000,
            dt_MesAno: "062014",
            dimensao: 10000,
            tp_naturezaObra: 1,
            tp_naturezaObra_c: 1,
            tp_Obra_c: 1,
            de_Sucinta_c: "Sucinta",
            pop_beneficiada: "benificiados aqui",
            foto: "Imagem em base64 aqui",
            foto_c: "qualquer coisa",
            dimensao_c: 100.02,
            cei : "CER3482JKO3892",
            crea: "CREA32984309PB",
            obs: "Observações aqui"
        },

        {
            cd_UGestora: 2,
            dt_Ano: 1992,
            nu_Obra: "325345353",
            dt_Cadastro: "1997-01-01 00:00:00",
            tp_Patrimonio: 'S',
            de_Localizacao: "Camara Municipal",
            de_Sucinta: "Reforma do telhado",
            tp_Obra: 24,
            tp_CategoriaObra: 1,
            tp_Previsto: "102007",
            dt_Inicio: "1987-01-01 00:00:00",
            dt_Conclusao: "2013-01-01 00:00:00",
            dt_Recebimento: "2014-01-01 00:00:00",
            tp_FonteObra: 1,
            vl_Obra: 423.0000,
            dt_MesAno: "062014",
            dimensao: 89429,
            tp_naturezaObra: 3,
            tp_naturezaObra_c: 4,
            tp_Obra_c: 5,
            de_Sucinta_c: "Sucinta",
            pop_beneficiada: "benificiados aqui",
            foto: "Imagem em base64 aqui",
            foto_c: "qualquer coisa",
            dimensao_c: 34214.02,
            cei : "CER3482JKO3892",
            crea: "CREA32984309PB",
            obs: "Observações aqui"
        },

        {
            cd_UGestora: 1,
            dt_Ano: 2003,
            nu_Obra: "00000000",
            dt_Cadastro: "2001-01-01 00:00:00",
            tp_Patrimonio: 'S',
            de_Localizacao: "Camara Municipal",
            de_Sucinta: "Construção de triplex para o presidente da casa",
            tp_Obra: 24,
            tp_CategoriaObra: 1,
            tp_Previsto: "102007",
            dt_Inicio: "2000-01-01 00:00:00",
            dt_Conclusao: "2013-01-01 00:00:00",
            dt_Recebimento: "2014-01-01 00:00:00",
            tp_FonteObra: 1,
            vl_Obra: 998.44,
            dt_MesAno: "062014",
            dimensao: 250000,
            tp_naturezaObra: 1,
            tp_naturezaObra_c: 1,
            tp_Obra_c: 1,
            de_Sucinta_c: "Sucinta",
            pop_beneficiada: "benificiados aqui",
            foto: "Imagem em base64 aqui",
            foto_c: "qualquer coisa",
            dimensao_c: 100.02,
            cei : "CER3482JKO3892",
            crea: "CREA32984309PB",
            obs: "Observações aqui"
        }

    ];

    /**
     * Obtém todas as obras do servidor.
     * @param {limit} Inteiro, é o número de observações que serão retornadas.
     * @param {offset} Inteiro, é o numero da primeira observação que será buscada.
     * @returns {HttpPromise} Promise da requisição.
     */
    this.getObras = function (limit, offset) {
        /* Usar esse aqui quando tivermos servidor */
        return $http.get(API_ENDPOINT + '?limit=' + limit + '&offset=' + offset);
        /* return $q.resolve(obrasMock); */
    };

}]);