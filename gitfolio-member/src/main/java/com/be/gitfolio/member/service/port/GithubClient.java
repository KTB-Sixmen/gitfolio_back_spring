package com.be.gitfolio.member.service.port;

import com.be.gitfolio.member.controller.response.MemberResponse;

import java.util.List;

import static com.be.gitfolio.member.controller.response.MemberResponse.*;

public interface GithubClient {

    List<MemberGithubRepository> getRepositoriesForUser(String username);

    List<String> getOrganizationsForUser(String username);

    List<MemberGithubRepository> getRepositoriesForOrganization(String org);
}
