package com.be.gitfolio.member;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;
import org.springframework.data.mongodb.config.EnableMongoAuditing;

@SpringBootApplication
@EnableJpaAuditing
@EnableMongoAuditing
@ComponentScan(basePackages = {"com.be.gitfolio.member", "com.be.gitfolio.common"})
public class GitfolioMemberApplication {

	public static void main(String[] args) {
		SpringApplication.run(GitfolioMemberApplication.class, args);
	}

}
