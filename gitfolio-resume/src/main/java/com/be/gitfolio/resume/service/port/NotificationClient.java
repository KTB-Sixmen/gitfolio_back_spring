package com.be.gitfolio.resume.service.port;

import com.be.gitfolio.common.event.KafkaEvent;
import reactor.core.Disposable;

import static com.be.gitfolio.common.event.KafkaEvent.*;

public interface NotificationClient {

    Disposable sendNotification(ResumeEvent resumeEvent);

}
