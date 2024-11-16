package com.be.gitfolio.auth.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.GenericToStringSerializer;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

@Configuration
public class AuthRedisConfig {

    private static final String CACHE_PREFIX = "auth:";

    @Bean
    public RedisTemplate<String, Object> authRedisTemplate(RedisConnectionFactory authConnectionFactory) {
        RedisTemplate<String, Object> template = new RedisTemplate<>();
        template.setConnectionFactory(authConnectionFactory);

        // Key에 프리픽스를 적용한 직렬화 설정
        template.setKeySerializer(keySerializer());
        // Value를 JSON 형식으로 직렬화
        template.setValueSerializer(valueSerializer());
        template.setHashKeySerializer(keySerializer());
        template.setHashValueSerializer(valueSerializer());

        template.afterPropertiesSet();
        return template;
    }

    // Key Serializer에 프리픽스 적용
    private RedisSerializer<String> keySerializer() {
        return new StringRedisSerializer() {
            @Override
            public byte[] serialize(String key) {
                return super.serialize(CACHE_PREFIX + key);
            }
        };
    }

    // Value Serializer 설정 (JSON 직렬화)
    private RedisSerializer<Object> valueSerializer() {
        return new GenericJackson2JsonRedisSerializer();
    }
}