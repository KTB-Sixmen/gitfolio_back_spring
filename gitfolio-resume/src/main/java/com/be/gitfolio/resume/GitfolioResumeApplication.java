package com.be.gitfolio.resume;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.mongodb.config.EnableMongoAuditing;

@SpringBootApplication
@EnableMongoAuditing
@EnableJpaAuditing
@EnableCaching
@ComponentScan(basePackages = {"com.be.gitfolio.resume", "com.be.gitfolio.common"})
public class GitfolioResumeApplication {

    public static void main(String[] args) {
        SpringApplication.run(GitfolioResumeApplication.class, args);
    }

}
