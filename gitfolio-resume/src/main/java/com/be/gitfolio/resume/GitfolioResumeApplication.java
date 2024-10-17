package com.be.gitfolio.resume;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.orm.jpa.HibernateJpaAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.mongodb.config.EnableMongoAuditing;

@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class, HibernateJpaAutoConfiguration.class})
@EnableMongoAuditing
@ComponentScan(basePackages = {"com.be.gitfolio.resume", "com.be.gitfolio.common"})
public class GitfolioResumeApplication {

    public static void main(String[] args) {
        SpringApplication.run(GitfolioResumeApplication.class, args);
    }

}
