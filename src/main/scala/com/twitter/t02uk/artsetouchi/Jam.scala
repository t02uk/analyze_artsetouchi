package com.twitter.t02uk.artsetouchi

case class Jam(
    gotDateTime: DateTime,
    time: String,
    island: String,
    venue: String,
    waitingTime: String,
    soldOut: Boolean,
    description: String
)