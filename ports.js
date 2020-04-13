let pessoasNoStorage = JSON.parse(localStorage.getItem('pessoas')) || [];

let app = Elm.Main.init({
    node: document.getElementById('app'),
    flags: JSON.stringify(pessoasNoStorage)
});

app.ports.cadastrar.subscribe((pessoa) =>{
    pessoasNoStorage = JSON.parse(localStorage.getItem('pessoas')) || [];

    pessoasNoStorage.push(pessoa)

    localStorage.setItem('pessoas', JSON.stringify(pessoasNoStorage));

    Swal.fire(
        pessoa.nome,
        'Cadastrado com sucesso',
        'success'
    );

})


