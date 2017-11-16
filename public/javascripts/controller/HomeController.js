angular.module('obrasPb').controller('HomeController', HomeController);

HomeController.$inject = ['ObrasService', 'numberOfBuildings'];

function HomeController(ObrasService, numberOfBuildings) {
    var self = this;

    self.fields = [
        {name: 'dt_Ano', description: "Ano"},
        {name: 'de_Localizacao', description: "Local"},
        {name: 'de_Sucinta', description: "Descrição"},
        {name: 'vl_Obra', description: "Valor em R$"}
    ];

    const DEFAULT_PAGE_SIZE = 5;
    const DEFAULT_START_PAGE = 1;
    const SHOWN_FIELDS = self.fields.map(field => field.name);

    var ORDER_BY_FIELD = 'cd_Ugestora';
    this.selectedObras = {};

    this.numberOfBuildings = numberOfBuildings;

    self.tableQuery = {
        order : ORDER_BY_FIELD,
        limit : DEFAULT_PAGE_SIZE,
        page : DEFAULT_START_PAGE,
        fields: SHOWN_FIELDS
    };

    this.getStartElement = function (page, limit) {
        return page * limit - limit + 1;
    };

    this.getObras = function () {
        self.obrasPromise = ObrasService.getObras(
            self.tableQuery.limit,
            self.getStartElement(self.tableQuery.page, self.tableQuery.limit),
            self.tableQuery.order,
            self.tableQuery.fields
        ).then(function (result) {
                self.obras = result.data.lista;
                self.obras = self.obras.map(function(obra) {
                    obra.fields = SHOWN_FIELDS;
                    return obra;
                });
            }).
            catch(function (err) {
                console.log(err);
            })
    };


    self.getObras();


}