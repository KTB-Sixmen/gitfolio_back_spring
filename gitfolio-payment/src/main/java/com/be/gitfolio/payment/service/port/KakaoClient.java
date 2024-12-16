package com.be.gitfolio.payment.service.port;

import java.util.Map;

public interface KakaoClient {

    <T> T sendKakaoPaymentRequest(String url, Map<String, ?> parameters, Class<T> responseType, String secretKey);
}
