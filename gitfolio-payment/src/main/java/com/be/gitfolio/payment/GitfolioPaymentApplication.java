package com.be.gitfolio.payment;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@SpringBootApplication
@ComponentScan(basePackages = {"com.be.gitfolio.payment", "com.be.gitfolio.common"})
@EnableJpaAuditing
public class GitfolioPaymentApplication {

    public static void main(String[] args) {
        SpringApplication.run(GitfolioPaymentApplication.class, args);
    }

}
