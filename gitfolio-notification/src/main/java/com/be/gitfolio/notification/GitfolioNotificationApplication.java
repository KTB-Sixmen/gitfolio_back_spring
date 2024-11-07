package com.be.gitfolio.notification;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@SpringBootApplication
@EnableJpaAuditing
@ComponentScan(basePackages = {"com.be.gitfolio.notification", "com.be.gitfolio.common"})
public class GitfolioNotificationApplication {

	public static void main(String[] args) {
		SpringApplication.run(GitfolioNotificationApplication.class, args);
	}

}
