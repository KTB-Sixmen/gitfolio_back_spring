package com.be.gitfolio.auth.dto;

import java.util.Map;

public class GithubResponse {

    private final Map<String, Object> attribute;

    public GithubResponse(Map<String, Object> attribute) {
        this.attribute = attribute;
    }

    public String getGithubId() {
        return attribute.get("id").toString();
    }

    // 사용자 이름
    public String getName() {
        return (String) attribute.get("login");
    }

    // 사용자 이메일
    public String getAvatarUrl() {
        return (String) attribute.get("avatar_url");
    }
}
