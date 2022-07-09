
var get_color_class = function(value){
    if (typeof(value) === "string") value = parseFloat(value);
    /* console.log(value); */
    if (value < 0.1) return "very-slow";
    else if (value < 0.5) return "slow";
    else if (value < 2) return "same";
    else if (value < 10) return "fast";
    else return "very-fast";
};

var writeBenchmarkTable = function(data, library, framework, scale, elt_type){
    table_div = document.getElementById("benchmark-table-div");
    if (framework === "all" && scale === "all"){
        table_div.innerHTML = `<p class="error">Only ONE of FRAMEWORK or SCALE can be 'All'</p>`;
        return false;
    }
    /* console.log(data); */
    let functions = data[library][0][elt_type];
    /* console.log(functions); */

    // first get the headers
    let header_values = null;
    if (framework === "all"){
        header_values = document.getElementsByName("framework")[0].options;
    }else{ // scale === "all"
        let headers = "";
        header_values = document.getElementsByName("scale")[0].options;
    }
    header_values = Array.from(header_values).map(h => h.value);


    let headers = "";
    for(let i=1; i<header_values.length; i++){
        headers += `<th>${header_values[i]}</th>\n`;
    }
    let header_row = `<tr>
                                 <th>Function Name</th>
                                 ${headers}
                               </tr>`;

    // Then prepare the rows
    let row_values = [];
    if (framework === "all"){
        for(let i=0; i<functions.length; i++){
            let fn = functions[i];
            /* console.log(fn); */
            let name = Object.keys(fn)[0];
            let scale_data = fn[name][scale];
            /* console.log(fn, scale, scale_data); */
            let row = [name];
            for(let j=1; j<header_values.length; j++){
                let header_value = header_values[j];
                row.push(scale_data[header_value]);
            }
            row_values.push(row);
        }
    }else{ // scale === "all"
        for(let i=0; i<functions.length; i++){
            let fn = functions[i];
            let name = Object.keys(fn)[0];
            let row = [name];
            for(let j=1; j<header_values.length; j++){
                let header_value = header_values[j];
                let framework_data = fn[name][header_value];
                row.push(framework_data[framework]);
            }
            row_values.push(row);
        }
    }

    /* console.log(row_values); */
    let rows = "";
    for(let i=0; i<row_values.length; i++){
        let row = row_values[i];
        /* console.log(row); */
        let cells = `<td>${row[0]}</td>`
        for(let j=1; j<row.length; j++){
            let cell = row[j];
            let cell_value = numeral(parseFloat(cell)).format("0.00");
            cells += `<td class="${get_color_class(cell)}">${cell_value}</td>`;
        }
        rows += `\n<tr>${cells}</tr>`;
    }
    /* console.log(rows); */

    // finally prepare the table
    table_div.innerHTML = `<table id="benchmark-table">${header_row}${rows}</table>`;
};

var updateBenchmarkTable = function(){
    let library   = document.getElementsByName("library")[0].value;
    let framework = document.getElementsByName("framework")[0].value;
    let scale = document.getElementsByName("scale")[0].value;
    let elt_type = document.getElementsByName("element-type")[0].value;

    console.log(library + " " + framework + " " + scale + " " + elt_type);
    fetch(library+".json")
        .then((response) => {
            if (response.ok){return response.json();}
            else {
                table_div = document.getElementById("benchmark-table-div");
                table_div.innerHTML = `<p class="error">The benchmark information for the selected combination seems to be absent.</p>`;
            }
        })
        .then(data => writeBenchmarkTable(data, library, framework, scale, elt_type));
};

window.onload = updateBenchmarkTable;
