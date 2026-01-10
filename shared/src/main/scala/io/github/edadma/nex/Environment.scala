package io.github.edadma.nex

import scala.collection.mutable

class Environment(parent: Option[Environment] = None):
  private val vars = mutable.Map[String, Value]()

  def set(name: String, value: Value): Unit =
    vars(name) = value

  def get(name: String): Option[Value] =
    vars.get(name).orElse(parent.flatMap(_.get(name)))

  def child: Environment = Environment(Some(this))
