package com.be.gitfolio.payment.dto;

public class KakaoResponse {

    // 카카오페이 결제 준비 응답
    public record KakaoReadyResponse(
            String tid,
            String next_redirect_app_url,
            String next_redirect_mobile_url,
            String next_redirect_pc_url,
            String android_app_scheme,
            String ios_app_scheme,
            String created_at
    ) {}

    // 카카오페이 결제 승인 응답
    public record KakaoApproveResponse(
            String aid, // 요청 고유 번호
            String tid, // 결제 고유 번호
            String cid, // 가맹점 코드
            String sid, // 정기결제용 ID
            String partner_order_id, // 가맹점 주문 번호
            String partner_user_id, // 가맹점 회원 ID
            String payment_method_type, // 결제 수단
            Amount amount, // 결제 금액 정보
            CardInfo card_info, // 카드 결제시 카드정보
            String item_name, // 상품명
            String item_code, // 상품 코드
            int quantity, // 상품 수량
            String created_at, // 결제 요청 시간
            String approved_at, // 결제 승인 시간
            String payload // 결제 승인 요청에 대해 저장 값, 요청 시 전달 내용
    ) {}

    // 카드 정보
    public record CardInfo(
            String kakaopay_purchase_corp,
            String kakaopay_purchase_corp_code,
            String kakaopay_issuer_corp,
            String kakaopay_issuer_corp_code,
            String bin,
            String card_type,
            String install_month,
            String approved_id,
            String card_mid,
            String interest_free_install,
            String installment_type,
            String card_item_code
    ) {}

    // 결제 금액 정보
    public record Amount(
            int total, // 총 결제 금액
            int tax_free, // 비과세 금액
            int tax, // 부가세 금액
            int point, // 사용한 포인트
            int discount, // 할인금액
            int green_deposit // 컵 보증금
    ) {}
}