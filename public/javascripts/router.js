angular.module('obrasPb').config(['$stateProvider', '$urlRouterProvider', function ($stateProvider, $urlRouterProvider) {

    $urlRouterProvider.when("", "/home");
    $urlRouterProvider.when("/", "/home");

    $urlRouterProvider.otherwise("/home");

    $stateProvider
        .state('home', {
            url: '/home',
            templateUrl: '/assets/views/home.html',
            controller: 'HomeController as homeCtrl',
            resolve: {
                obras: function (ObrasService) {
                    return ObrasService.getObras().then(function (data) {
                        return data;
                    });
                }
            }
        })

}]);