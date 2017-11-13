angular.module('obrasPb').service('ObrasService', ['$http', '$q', function ($http, $q) {

    var self = this;
    const API_ENDPOINT = "/buildings";

    /**
     * Obtém todas as obras do servidor.
     * @param {limit} Inteiro, é o número de observações que serão retornadas.
     * @param {offset} Inteiro, é o numero da primeira observação que será buscada.
     * @returns {HttpPromise} Promise da requisição.
     */
    this.getObras = function (limit, offset) {
        return $http.get(API_ENDPOINT + '?limit=' + limit + '&offset=' + offset + '&metadata=0');
    };

    /**
     * Obtém o número total de obras cadastradas.
     * @returns {HttpPromise} Promise da requisição
     */
    this.getNumberOfBuildings = function (callback) {
        return $http.get(API_ENDPOINT + '?limit=1&offset=0&metadata=1').then(function (result) {
            return callback(result.data.count);
        })
    };

}]);