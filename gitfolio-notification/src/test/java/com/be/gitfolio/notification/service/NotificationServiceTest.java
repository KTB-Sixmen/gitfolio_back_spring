package com.be.gitfolio.notification.service;

import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.type.NotificationType;
import com.be.gitfolio.notification.domain.Notification;
import com.be.gitfolio.notification.mock.FakeNotificationRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static com.be.gitfolio.common.event.KafkaEvent.*;
import static com.be.gitfolio.notification.dto.NotificationResponseDTO.*;
import static org.assertj.core.api.Assertions.*;

public class NotificationServiceTest {

    private NotificationService notificationService;

    @BeforeEach
    void init() {
        FakeNotificationRepository fakeNotificationJpaRepository = new FakeNotificationRepository();
        notificationService = new NotificationService(fakeNotificationJpaRepository);
        fakeNotificationJpaRepository.save(Notification.builder()
                .id(1L)
                .senderNickname("fakeSender")
                .senderId(2L)
                .receiverId(1L)
                .type(NotificationType.LIKE)
                .read(Boolean.FALSE)
                .resumeId("fakeResumeId")
                .build());
        fakeNotificationJpaRepository.save(Notification.builder()
                .id(2L)
                .senderNickname("fakeSender")
                .senderId(2L)
                .receiverId(1L)
                .type(NotificationType.LIKE)
                .read(Boolean.TRUE)
                .resumeId("fakeResumeId")
                .build());
    }

    @Test
    void 이벤트가_도착하면_ResumeEvent객체로_notification를_생성할_수_있다() throws Exception {
        //given
        ResumeEvent resumeEvent = ResumeEvent.builder()
                .senderId(1L)
                .senderNickname("test123")
                .resumeId("resumeId123")
                .receiverId(2L)
                .type(NotificationType.LIKE)
                .build();
        //when
        Notification notification = notificationService.createNotification(resumeEvent);
        //then
        assertThat(notification.getId()).isPositive();
    }

    @Test
    void 사용자_ID_값으로_읽지_않은_내_알림_전체_조회를_할_수_있다() throws Exception {
        //given
        //when
        List<NotificationListDTO> myNotificationList = notificationService.getMyNotificationList(1L);
        //then
        assertThat(myNotificationList).hasSize(1);
    }

    @Test
    void 사용자_ID_값과_알림_ID로_알림을_단건_조회할_수_있고_읽음_처리할_수_있다() throws Exception {
        //given
        //when
        NotificationListDTO notification = notificationService.getNotification(1L, 1L);
        //then
        assertThat(notification.read()).isTrue();
        assertThat(notification.senderNickName()).isEqualTo("fakeSender");
    }

    @Test
    void 다른_사용자의_알림에_임의로_접근해_조회하려고_하면_예외가_발생한다() throws Exception {
        //given
        //when
        //then
        assertThatThrownBy(() -> {
            notificationService.getNotification(3L, 1L);
        }).isInstanceOf(BaseException.class);
    }
}
