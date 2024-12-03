package com.be.gitfolio.notification.domain;

import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.common.type.NotificationType;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import static com.be.gitfolio.common.event.KafkaEvent.*;
import static org.assertj.core.api.Assertions.*;

public class NotificationTest {

    @Test
    void ResumeEvent로_새로운_알림객체를_만들_수_있다() throws Exception {
        //given
        ResumeEvent resumeEvent = ResumeEvent.builder()
                .senderId(1L)
                .senderNickname("test123")
                .resumeId("resumeId123")
                .receiverId(2L)
                .type(NotificationType.LIKE)
                .build();
        //when
        Notification notification = Notification.from(resumeEvent);
        //then
        assertThat(notification.getReceiverId()).isEqualTo(2L);
        assertThat(notification.getResumeId()).isEqualTo("resumeId123");
        assertThat(notification.getType()).isEqualTo(NotificationType.LIKE);
    }

    @Test
    void Notification의_read_상태를_변경할_수_있다() throws Exception {
        //given
        Notification notification = Notification.builder()
                .read(Boolean.FALSE)
                .build();
        //when
        notification.readNotification();
        //then
        assertThat(notification.isRead()).isTrue();
    }
}
