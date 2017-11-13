angular.module('obrasPb').controller('HomeController', ['ObrasService', 'numberOfBuildings', function (ObrasService, numberOfBuildings) {
    var self = this;

    const DEFAULT_PAGE_SIZE = 5;
    const DEFAULT_START_PAGE = 1;

    this.selectedObras = {};
    this.numberOfBuildings = numberOfBuildings;


    this.tableQuery = {
        order : 'dt_Ano',
        limit : DEFAULT_PAGE_SIZE,
        page : DEFAULT_START_PAGE
    };

    this.getStartElement = function (page, limit) {
        return page * limit - limit + 1;
    };

    this.getObras = function () {
        self.obrasPromise = ObrasService.getObras(
            self.tableQuery.limit,
            self.getStartElement(self.tableQuery.page, self.tableQuery.limit)
        ).then(function (result) {
            self.obras = result.data.lista;
        }).
          catch(function (err) {
            console.log(err);
        })
    };


    self.getObras();

}]);