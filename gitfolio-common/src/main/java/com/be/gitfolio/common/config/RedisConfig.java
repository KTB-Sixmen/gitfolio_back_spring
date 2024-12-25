package com.be.gitfolio.common.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.RedisSentinelConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;

@Configuration
public class RedisConfig {

    @Bean
    public LettuceConnectionFactory redisConnectionFactory() {
        RedisSentinelConfiguration sentinelConfig = new RedisSentinelConfiguration()
                .master("mymaster")
                .sentinel("gitfolio-redis-node-0.gitfolio-redis-headless", 26379)
                .sentinel("gitfolio-redis-node-1.gitfolio-redis-headless", 26379)
                .sentinel("gitfolio-redis-node-2.gitfolio-redis-headless", 26379);

        return new LettuceConnectionFactory(sentinelConfig);
    }
}
