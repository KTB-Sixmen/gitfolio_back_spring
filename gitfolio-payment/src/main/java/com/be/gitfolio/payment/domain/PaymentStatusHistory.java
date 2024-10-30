package com.be.gitfolio.payment.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
import com.be.gitfolio.payment.type.PaymentStatus;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Entity
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PaymentStatusHistory {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "payment_status_history_id")
    private Long id;

    private Long paymentId;

    @Enumerated(EnumType.STRING)
    private PaymentStatus status;

    private LocalDateTime changedAt;    // 변경 일자

    private String reason;  // 상태 변경 사유 (예: 결제 실패 사유)
}
