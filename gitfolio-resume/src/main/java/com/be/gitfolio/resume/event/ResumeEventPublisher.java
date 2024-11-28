package com.be.gitfolio.resume.event;

import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.common.type.NotificationType;
import lombok.RequiredArgsConstructor;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

import static com.be.gitfolio.common.event.KafkaEvent.*;

@Component
@RequiredArgsConstructor
public class ResumeEventPublisher {

    private final KafkaTemplate<String, Object> kafkaTemplate;

    // 댓글 및 좋아요 이벤트 발행
    public void publishResumeEvent(Long senderId, String senderNickname, Long receiverId, String resumeId, NotificationType type) {
        ResumeEvent event = ResumeEvent.builder()
                .senderId(senderId)
                .senderNickname(senderNickname)
                .receiverId(receiverId)
                .resumeId(resumeId)
                .type(type)
                .build();
        kafkaTemplate.send("resumeEventTopic", event);
    }
}
