angular.module('obrasPb').controller('HomeController', ['obras', function (obras) {
    var self = this;
    this.obras = obras;
}]);