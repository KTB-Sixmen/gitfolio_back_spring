package com.be.gitfolio.payment;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@ComponentScan(basePackages = {"com.be.gitfolio.payment", "com.be.gitfolio.common"})
@EnableJpaAuditing
@EnableScheduling
public class GitfolioPaymentApplication {

    public static void main(String[] args) {
        SpringApplication.run(GitfolioPaymentApplication.class, args);
    }

}
