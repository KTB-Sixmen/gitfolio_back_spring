package com.be.gitfolio.payment.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.payment.config.KakaoPayProperties;
import com.be.gitfolio.payment.domain.Payment;
import com.be.gitfolio.payment.domain.PaymentStatusHistory;
import com.be.gitfolio.payment.repository.PaymentRepository;
import com.be.gitfolio.payment.repository.PaymentStatusHistoryRepository;
import com.be.gitfolio.payment.type.PaymentStatus;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.HashMap;
import java.util.Map;

import static com.be.gitfolio.payment.dto.PaymentRequest.*;
import static com.be.gitfolio.payment.dto.PaymentResponse.*;


@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
@Slf4j
public class PaymentService {

    @Value("${payment.server.url}")
    private String paymentBaseUrl;
    private final PaymentRepository paymentRepository;
    private final PaymentStatusHistoryRepository paymentStatusHistoryRepository;
    private final KakaoPayProperties payProperties;
    private final WebClient kakaoWebClient;
    private final WebClient memberWebClient;
    private KakaoReadyResponse kakaoReady;

    private HttpHeaders getHeaders() {
        HttpHeaders headers = new HttpHeaders();
        String auth = "SECRET_KEY " + payProperties.getSecretKey();
        headers.set("Authorization", auth);
        headers.set("Content-Type", "application/json");
        return headers;
    }

    /**
     * 결제 준비
     * @param memberId
     * @param paidPlanRequest
     * @return KakaoReadyResponse
     */
    @Transactional
    public KakaoReadyResponse kakaoPayReady(Long memberId, PaidPlanRequest paidPlanRequest) {
        Map<String, Object> parameters = createPaymentParameters(memberId, paidPlanRequest.paidPlan());

        // 카카오페이 결제 준비 API 호출
        kakaoReady = sendKakaoPaymentRequest(
                "https://open-api.kakaopay.com/online/v1/payment/ready",
                parameters,
                KakaoReadyResponse.class
        );

        Payment payment = Payment.of(memberId, paidPlanRequest, kakaoReady);
        paymentRepository.save(payment);

        PaymentStatusHistory paymentStatusHistory = PaymentStatusHistory.ready(payment.getId());
        paymentStatusHistoryRepository.save(paymentStatusHistory);

        return kakaoReady;
    }

    /**
     * 결제 완료 승인
     * @param memberId
     * @param pgToken
     * @return KakaoApproveResponse
     */
    @Transactional
    public KakaoApproveResponse approveResponse(Long memberId, String pgToken) {
        KakaoApproveResponse kakaoApproveResponse = confirmKakaoPayment(memberId, pgToken);

        Payment payment = paymentRepository.findByTransactionId(kakaoApproveResponse.tid())
                .orElseThrow(() -> new BaseException(ErrorCode.NO_PAYMENT_TRANSACTION_INFO));
        // 결제 상세 정보 추가 및 상태 변경
        payment.updatePaymentDetails(kakaoApproveResponse, PaymentStatus.APPROVED);
        paymentRepository.save(payment);
        // 결제 상태 변경 히스토리 저장
        PaymentStatusHistory paymentStatusHistory = PaymentStatusHistory.approve(payment.getId());
        paymentStatusHistoryRepository.save(paymentStatusHistory);

        memberWebClient.patch()
                .uri("/api/members/{memberId}/updatePlan", memberId)
                .bodyValue(PaidPlan.fromPlanName(kakaoApproveResponse.item_name()))
                .retrieve()
                .toBodilessEntity()
                .block();

        return kakaoApproveResponse;
    }

    /**
     * 결제 취소
     * @param memberId
     * @param paidPlan
     */
    @Transactional
    public void cancelPayment(Long memberId, PaidPlan paidPlan) {
        Payment payment = paymentRepository.findByMemberIdAndPaidPlan(memberId, paidPlan)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_PAYMENT_TRANSACTION_INFO));

        PaymentStatusHistory paymentStatusHistory = PaymentStatusHistory.cancel(payment.getId(), "결제중 취소");
        paymentStatusHistoryRepository.save(paymentStatusHistory);
    }

    /**
     * 결제 실패
     * @param memberId
     * @param paidPlan
     */
    @Transactional
    public void failPayment(Long memberId, PaidPlan paidPlan) {
        Payment payment = paymentRepository.findByMemberIdAndPaidPlan(memberId, paidPlan)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_PAYMENT_TRANSACTION_INFO));

        PaymentStatusHistory paymentStatusHistory = PaymentStatusHistory.fail(payment.getId(), "결제 실패");
        paymentStatusHistoryRepository.save(paymentStatusHistory);
    }

    /**
     * 정기 결제 해지
     * @param memberId
     */
    @Transactional
    public void terminatePayment(Long memberId) {
        Payment payment = paymentRepository.findByMemberId(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_PAYMENT_INFO));

        Map<String, Object> parameters = new HashMap<>();
        parameters.put("cid", "TCSUBSCRIP");
        parameters.put("sid", payment.getSid());

        sendKakaoPaymentRequest(
                "https://open-api.kakaopay.com/online/v1/payment/manage/subscription/inactive",
                parameters,
                Void.class
        );

        payment.terminate();
        paymentRepository.save(payment);

        PaymentStatusHistory paymentStatusHistory = PaymentStatusHistory.cancel(payment.getId(), "정기 결제 해지");
        paymentStatusHistoryRepository.save(paymentStatusHistory);
    }


    // 결제 요청 파라미터 생성
    private Map<String, Object> createPaymentParameters(Long memberId, PaidPlan paidPlan) {
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("cid", payProperties.getCid());
        parameters.put("partner_order_id", "1011");
        parameters.put("partner_user_id", String.valueOf(memberId));
        parameters.put("item_name", paidPlan.getPlanName());
        parameters.put("quantity", "1");
        parameters.put("total_amount", String.valueOf(paidPlan.getCost()));
        parameters.put("tax_free_amount", "0");
        parameters.put("approval_url", paymentBaseUrl + "/api/payments/success?member_id=" + memberId);
        parameters.put("fail_url", paymentBaseUrl + "/api/payments/fail?member_id=" + memberId + "&paid_plan=" + paidPlan);
        parameters.put("cancel_url", paymentBaseUrl + "/api/payments/cancel?member_id=" + memberId + "&paid_plan=" + paidPlan);
        return parameters;
    }

    // 카카오페이 결제 승인 API 호출
    private KakaoApproveResponse confirmKakaoPayment(Long memberId, String pgToken) {
        Map<String, String> parameters = new HashMap<>();
        parameters.put("cid", payProperties.getCid());
        parameters.put("tid", kakaoReady.tid());
        parameters.put("partner_order_id", "1011");
        parameters.put("partner_user_id", String.valueOf(memberId));
        parameters.put("pg_token", pgToken);

        return sendKakaoPaymentRequest(
                "https://open-api.kakaopay.com/online/v1/payment/approve",
                parameters,
                KakaoApproveResponse.class
        );
    }

    // 카카오페이 API 요청 전송
    private <T> T sendKakaoPaymentRequest(String url, Map<String, ?> parameters, Class<T> responseType) {
        return kakaoWebClient.post()
                .uri(url)
                .headers(headers -> headers.addAll(getHeaders()))
                .bodyValue(parameters)
                .retrieve()
                .bodyToMono(responseType)
                .block(); // 비동기 방식 사용 시 block() 제거 가능
    }
}
