import es.ES
import play.api.libs.logback.LogbackLoggerConfigurator
import play.api.{Application, ApplicationLoader}
import play.api.ApplicationLoader.Context
import ch.qos.logback.classic.LoggerContext
import ch.qos.logback.classic.spi.ILoggingEvent
import me.moocar.logbackgelf.{GZIPEncoder, GelfLayout, GelfUDPAppender}
import org.slf4j.LoggerFactory

class AppLoader extends ApplicationLoader {

  private val logger = LoggerFactory.getLogger(getClass)

  override def load(context: Context): Application = {
    val config = Config.unsafeLoad()
    logger.info(s"Loaded configuration: $config")

    new LogbackLoggerConfigurator().configure(context.environment)
    val loggingToGraylog = enableGraylogLogging(config.logging)
    logger.info(s"Logging to Graylog? $loggingToGraylog")

    val components = new AppComponents(context, config)

    ES.initIndex.run(components.jestClient)

    components.application
  }

  private def enableGraylogLogging(config: LoggingConfig): Boolean = config match {
    case GraylogEnabledLoggingConfig(graylogHostname, myHostname) =>
      addGraylogAppender(graylogHostname, myHostname)
      true
    case GraylogDisabledLoggingConfig =>
      false
  }

  private def addGraylogAppender(graylogHostname: String, myHostname: String): Unit = {
    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

    val layout = new GelfLayout[ILoggingEvent]
    layout.setContext(loggerContext)
    layout.setHost(myHostname)
    layout.addStaticAdditionalField("service:shipit")
    layout.setIncludeFullMDC(true)
    layout.setUseThreadName(true)
    layout.start()

    val encoder = new GZIPEncoder[ILoggingEvent]
    encoder.setContext(loggerContext)
    encoder.setLayout(layout)

    val appender = new GelfUDPAppender[ILoggingEvent]
    appender.setContext(loggerContext)
    appender.setRemoteHost(graylogHostname)
    appender.setPort(12201)
    appender.setEncoder(encoder)
    appender.setName("graylog")
    appender.start()

    loggerContext.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).addAppender(appender)
  }

}
