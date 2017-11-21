angular.module('obrasPb').controller('HomeController', HomeController);

HomeController.$inject = ['ObrasService'];

function HomeController(ObrasService) {
    var self = this;

    self.fields = [
        {name: 'cd_UGestora', description: 'Id Unidade Gestora'},
        {name: 'de_UGestora', description : "Unidade Gestora"},
        {name: 'dt_Ano', description: "Ano"},
        {name: 'de_Localizacao', description: "Local"},
        {name: 'de_Sucinta', description: "Descrição"},
        {name: 'vl_Obra', description: "Valor em R$"}
    ];

    const DEFAULT_PAGE_SIZE = 5;
    const DEFAULT_START_PAGE = 1;
    const SHOWN_FIELDS = self.fields.map(field => field.name);
    const COUNT_BUILDINGS_HEADER = "x-total-count";

    var ORDER_BY_FIELD = 'cd_UGestora';

    self.tableQuery = {
        order : ORDER_BY_FIELD,
        limit : DEFAULT_PAGE_SIZE,
        page : DEFAULT_START_PAGE,
        fields: SHOWN_FIELDS,
        filterBy: ""
    };

    this.getObras = function () {
        self.obrasPromise = ObrasService.getObras(
            self.tableQuery.limit,
            self.tableQuery.page,
            self.tableQuery.order,
            self.tableQuery.fields,
            self.tableQuery.filterBy
        ).then(function (result) {
                self.obras = result.data.lista;
                self.numberOfBuildings = result.headers(COUNT_BUILDINGS_HEADER);
                self.obras = self.obras.map(function(obra) {
                    obra.fields = SHOWN_FIELDS;
                    return obra;
                });
            }).
            catch(function (err) {
                console.log(err);
            });
    };


    this.setOrderingField = function(field) {
        if (self.tableQuery.order == field && self.tableQuery.order[0] != "-") {
            self.tableQuery.order = "-" + field;
        } else {
            self.tableQuery.order = field;
        }
    };


    self.getObras();

}