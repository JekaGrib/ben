# What is it
Ben is echo-bot application for Telegram and Vk.
# What it can do
Bot can send a message from the user to him back. Bot send response message several times. This number is set by the user.
# Bot commands
| Command              | Action                             |
| :------------------- | :--------------------------------- |
| /help                | bot send info about itself         | 
| /repeat              | bot send keyboard, where user can change number of repeats messages. Keyboard also has info msg about current number of repeats        |
# Types of messages sent
| Telegram             | Vk                                 |
| :------------------- | :--------------------------------- |
| all types            | Text, sticker, photo, doc, audioMessage, video, audio, market, wall, poll         | 

Unsupported types will be ignored
# Installing
You can install bot with:

    $ git clone https://github.com/JekaGrib/ben.git
    $ stack build

# Getting started
## 1. Create your bot
You should create your bot in Tg or Vk.

Necessary info for Tg: https://core.telegram.org/bots#3-how-do-i-create-a-bot

Necessary info for Vk: https://vk.com/dev/bots_docs

When you create a bot, you will receive a token
## 2. Configuration
Before start, you should rename "example.config" to "bot.config". 
Then you should make changes in this file.

There is table with descriptions of each values in configuration file, that should be replace to your values.

|App  | Value                | Description                            | 
|:----| :------------------- |:----------------------------------------- |
|Tg,Vk| startN               | Default number of repeats messages. This number is set automatically, when user start work with bot.  This number can then be changed by the user with the command "/repeat". It can be whole number from 1 to 5|
|Tg,Vk| botToken             | Bot token                                 |
|Tg,Vk| logLevel             | The logging level is specified here. The log will only display entries of this level and levels above. It can be one of four levels: DEBUG,INFO,WARNING,ERROR. More information [here](#logging)  |
|Tg,Vk| help_Info_Msg        | The message that is sent to the user for the command "/help" |
|Tg,Vk| repeat_Info_Question | The question that is sent to the user for the command "/repeat" |
| Vk  | group_id             | Id of bot group                           |

## 3. Run
You can run Tg bot with:

    $ stack exec ben-exe  Tg

You can run Vk bot with:

    $ stack exec ben-exe  Vk

# Logging

There are 4 logging levels from lowest to highest:

1. DEBUG
2. INFO
3. WARNING
4. ERROR

The logging level is specified in [Configuration](#2-configuration). The log will only display entries of this level and levels above.

# Structure

![](https://github.com/JekaGrib/ben/raw/master/pic/structure.png)

Application has two main parts. For Vk and Tg messengers.

Conf - module with configuration
Logger - module with logging (has handler)
Oops - module with exceptions processing 
Api - folder with parsing requests and responses modules
App - module with bot main functionality (has handler)
PrepareAttachment - module with prepare attachment messages functionality for Vk (has handler)

Modules, which has handlers, have [Tests](#tests)

# Test

Modules, which has handlers, have unit-tests:

1. Tg.App.hs
1. Vk.App.hs
1. Vk.App.PrepareAttachment.hs

