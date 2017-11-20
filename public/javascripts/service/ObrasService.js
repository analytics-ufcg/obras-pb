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

}]);