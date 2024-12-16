package com.be.gitfolio.auth.infrastructure;

import lombok.RequiredArgsConstructor;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Repository;

import java.util.concurrent.TimeUnit;

@Repository
@RequiredArgsConstructor
public class RedisTokenRepository {

    private final RedisTemplate<String, Object> authRedisTemplate;

    // Refresh 토큰 저장
    public void saveRefreshToken(String key, String refreshToken, Long expirationTimeMs) {
        authRedisTemplate.opsForValue().set(key, refreshToken, expirationTimeMs, TimeUnit.MILLISECONDS);
    }

    // Refresh 토큰 조회
    public String getRefreshToken(String key) {
        return (String) authRedisTemplate.opsForValue().get(key);
    }

    // Refresh 토큰 삭제
    public void deleteRefreshToken(String key) {
        authRedisTemplate.delete(key);
    }

    // Refresh 토큰 존재 여부 확인
    public boolean existsByRefreshToken(String key) {
        return authRedisTemplate.hasKey(key);
    }

}
