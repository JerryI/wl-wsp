# WolframScript Pages
Hypertext preprocessor (template engine) built on top of Wolfram Kernel

2023 update! __please consider to use a successor of WSP -__ [__Wolfam Language XML__](https://github.com/JerryI/wl-wlx)

## Official Wolfram Paclet Page
[Link](https://resources.wolframcloud.com/PacletRepository/resources/JerryI/WSP/)
```mathematica
PacletInstall["JerryI/WSP"]
```

> See more examples @ [TinyWeb](https://github.com/JerryI/tinyweb-mathematica)

Imagine PHP or JSP, but using Wolfram Language. Easy to use, feels like plain HTML
```php
<?wsp Now//TextString ?>
Thu 22 Jun 2023 23:43:21
```

Use together with modules

*index.wsp*
```php
<body>
    <?wsp LoadPage["c/mycomponent.wsp", {title = "Title"}] ?>
</body>
```

*c/mycomponent.wsp*
```php
<h1><?wsp title ?></h1>
```

or any Wolfram Language expressions

*tables*
```php
<ul>
<?wsp Table[ ?>
    <li><?wsp RandomWord[] ?></li>
<?wsp , {i, 10}] >
</ul>
```
