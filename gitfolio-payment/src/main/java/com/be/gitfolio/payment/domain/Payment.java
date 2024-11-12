package com.be.gitfolio.payment.domain;


import com.be.gitfolio.common.config.BaseEntityMySQL;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.payment.dto.PaymentRequest;
import com.be.gitfolio.payment.dto.PaymentResponse;
import com.be.gitfolio.payment.type.PaymentStatus;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

import static com.be.gitfolio.payment.dto.PaymentRequest.*;
import static com.be.gitfolio.payment.dto.PaymentResponse.*;

@Entity
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Payment extends BaseEntityMySQL {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "payment_id")
    private Long id;

    private String transactionId;  // 카카오페이에서 반환된 결제 TID

    private Long memberId;         // 사용자 ID

    private BigDecimal amount;     // 결제 금액

    private String paymentType; // 결제 수단

    private String cardType;    // 카드 결제 시 카드 타입 (체크, 신용)

    private String cardCorp;    // 카카오페이 발급사명

    private String cardApprovedId;  // 카드사 승인번호

    @Enumerated(EnumType.STRING)
    private PaidPlan paidPlan;  // 플랜

    @Enumerated(EnumType.STRING)
    private PaymentStatus status;  // 결제 상태

    public static Payment of(Long memberId, PaidPlanRequest paidPlanRequest, KakaoReadyResponse kakaoReady) {
        return Payment.builder()
                .transactionId(kakaoReady.tid())
                .memberId(memberId)
                .amount(new BigDecimal(paidPlanRequest.paidPlan().getCost()))
                .paidPlan(paidPlanRequest.paidPlan())
                .status(PaymentStatus.PENDING)
                .build();
    }

    public void updatePaymentDetails(KakaoApproveResponse kakaoApproveResponse, PaymentStatus paymentStatus) {
        this.paymentType = kakaoApproveResponse.payment_method_type();
        if (kakaoApproveResponse.card_info() != null) {
            this.cardType = kakaoApproveResponse.card_info().card_type();
            this.cardCorp = kakaoApproveResponse.card_info().kakaopay_issuer_corp();
            this.cardApprovedId = kakaoApproveResponse.card_info().approved_id();
        }
        this.status = paymentStatus;
    }
}
