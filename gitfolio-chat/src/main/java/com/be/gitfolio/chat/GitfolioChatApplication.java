package com.be.gitfolio.chat;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.mongodb.config.EnableMongoAuditing;

@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class})
@EnableMongoAuditing
@ComponentScan(basePackages = {"com.be.gitfolio.chat", "com.be.gitfolio.common"})
public class GitfolioChatApplication {

    public static void main(String[] args) {
        SpringApplication.run(GitfolioChatApplication.class, args);
    }

}
