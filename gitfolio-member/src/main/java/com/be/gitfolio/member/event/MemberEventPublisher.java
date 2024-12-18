package com.be.gitfolio.member.event;

import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.common.type.NotificationType;
import lombok.RequiredArgsConstructor;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.stereotype.Component;

import static com.be.gitfolio.common.event.KafkaEvent.*;

@Component
@RequiredArgsConstructor
public class MemberEventPublisher {

    private final KafkaTemplate<String, Object> kafkaTemplate;

    // 댓글 및 좋아요 이벤트 발행
    public void publishResumeEvent(Long memberId, String username) {
        MemberDeletedEvent event = MemberDeletedEvent.builder().memberId(memberId).username(username).build();
        kafkaTemplate.send("memberDeletedEventTopic", event);
    }
}
