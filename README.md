# Erlang Xslt & Xml (Что это)

Простой многопоточный xslt-преобразователь.
В комлекте прилагается сериализатор в xml.

# Requirements (Что нужно)

* libxml2
* libxslt
* g++
* gnumake
* erlang
* rebar

# Options (Настройка)

    * В `./rebar.conf` (`DRV_LDFLAGS`) можно стереть `-DDEBUG`
        тем самым вы включите кеширование шаблонов.write_cmd

# Example (Пример)

## Xslt

    Xml = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    "<catalog>"
    "<cd>"
        "<title>Empire Burlesque</title>"
        "<artist>Bob Dylan</artist>"
        "<country>USA</country>"
        "<company>Columbia</company>"
        "<price>10.90</price>"
        "<year>1985</year>"
        "</cd>"
    "</catalog>",

    xslt:start_link(),
    xslt:apply("priv/xsl/template.xsl", Xml).

## Xml

### Seq

    >>> xml:encode_seq({data, "some data"}).
    "<data>some data</data>"
    >>> xml:encode_seq({data, [a, b, c]})
    "<data><item>a</item><item>b</item><item>c</item></data>",
    >>> xml:encode_seq({data,{more, 1}})
    "<data><more>1</more></data>"

### Para

    >>> xml:start_link().
    >>> xml:encode({data, "some data"}).
    "<data>some data</data>"
    >>> xml:encode({data, [a, b, c]})
    "<data><item>a</item><item>b</item><item>c</item></data>",
    >>> xml:encode({data,{more, 1}})
    "<data><more>1</more></data>"

# Credis (Кто это натворил)

* Сергей Кожевников (Serge Kozhevnikov aka cff, 2011);
* Илья w-495 Никитин (w-495, 2012).

# TODO

    * Документировать код.
    * Переписать части написанные с помощью с++ на чистый c
        (хинт: суффиксные деревья, хеширование методом половины квадрата).
    * Оформить в виде отдельного OTP приложения.
    * Оформить в виде отдельной ноды.
