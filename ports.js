let pessoasNoStorage = JSON.parse(localStorage.getItem('pessoas')) || [];

let app = Elm.Main.init({
    node: document.getElementById('app'),
    flags: JSON.stringify(pessoasNoStorage)
});

// app.ports.cadastrar.subscribe((pessoa) =>{
//     pessoasNoStorage = JSON.parse(localStorage.getItem('pessoas')) || [];

//     pessoasNoStorage.push(pessoa)

//     localStorage.setItem('pessoas', JSON.stringify(pessoasNoStorage));

//     Swal.fire(
//         pessoa.nome,
//         'Cadastrado com sucesso',
//         'success'
//     );

// });


// app.ports.editar.subscribe((pessoa) =>{
//     pessoasNoStorage = JSON.parse(localStorage.getItem('pessoas')) || [];

// })

app.ports.apagar.subscribe((pessoa) =>{

    Swal.fire({
        title: 'Certeza que deseja excluir?',
        text: pessoa.nome,
        icon: 'warning',
        showCancelButton: true,
        confirmButtonColor: '#3085d6',
        cancelButtonColor: '#d33',
        cancelButtonText: 'Cancelar',
        confirmButtonText: 'Excluir'
    }).then((result) => {

        if (result.value) {

            app.ports.pessoaApagada.send(pessoa);

            Swal.fire(
                'Excluído!',
                'Pessoa excluída com sucesso',
                'success'
            );
        }
    });

})


app.ports.toJs.subscribe((listaPessoas) =>{

    localStorage.setItem('pessoas', JSON.stringify(listaPessoas));

})
