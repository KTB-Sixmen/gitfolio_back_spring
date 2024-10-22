package com.be.gitfolio.auth;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;

@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class, HibernateJpaAutoConfiguration.class})
@ComponentScan(basePackages = {"com.be.gitfolio.auth", "com.be.gitfolio.common"})
public class GitfolioAuthApplication {

    public static void main(String[] args) {
        SpringApplication.run(GitfolioAuthApplication.class, args);
    }

}
