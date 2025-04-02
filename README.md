# loggman

## Install

```
$ brew tap kk0000-kk/loggman
$ brew install loggman

```

## Usage

### start loggman

```
$ loggman hoge.md
$ Template written to ./hoge.md

=====================
Current file content:

## TODO

## LOG
>
```

### add log

```
> hogehoge
Text has been written to ./hoge.md

=====================
Current file content:

## TODO

## LOG

2025-03-30(Sun) 16:50:48
hogehoge
>
```

### edit todo

```
> todo
```

- Open the file with the vi editor
- Edit the content and close the editor to return to the input prompt

```
## TODO

- [ ] fuga
- [ ] piyo

## LOG

2025-03-30(Sun) 16:50:48
hogehoge

2025-03-30(Sun) 16:50:53
タスクばらし/start

2025-03-30(Sun) 16:51:04
タスクばらし/stop
>
```

### done todo

```
> done fuga
```

- Check off TODO items that match the beginning of the input text

### exit

```
> exit
```

- Exit and the exit timestamp will be recorded
