angular.module('obrasPb').service('ObrasService', ['$http', '$q', function ($http, $q) {

    var self = this;
    const API_ENDPOINT = "/buildings";
    const COUNT_BUILDINGS_HEADER = "x-total-count";

    /**
     * Obtém todas as obras do servidor.
     * @param {limit} Inteiro, é o número de observações que serão retornadas.
     * @param {offset} Inteiro, é o numero da primeira observação que será buscada.
     * @returns {HttpPromise} Promise da requisição.
     */
    this.getObras = function (limit, page, orderingField, fields) {
        return $http.get(API_ENDPOINT + '?limit=' + limit + '&page=' + page + '&metadata=1' + "&orderingField=" +
        orderingField + "&fields=" + fields.join(","));
    };

    /**
     * Obtém o número total de obras cadastradas.
     * @param {callback} Função que será chamada com o total de observações quando a promise for resolvida.
     * @returns {HttpPromise} Promise da requisição
     */
    this.getNumberOfBuildings = function (callback) {
        return $http.get(API_ENDPOINT).then(function (result) {
            return callback(result.headers(COUNT_BUILDINGS_HEADER));
        })
    };

}]);