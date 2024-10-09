package com.be.gitfolio.auth.dto;

import com.be.gitfolio.auth.dto.MemberDTO;
import com.be.gitfolio.auth.dto.MemberDTO.OAuth2UserDTO;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.oauth2.core.user.OAuth2User;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

@RequiredArgsConstructor
public class CustomOAuth2User implements OAuth2User {

    private final OAuth2UserDTO memberDTO;

    @Override
    public Map<String, Object> getAttributes() {
        return null;
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        Collection<GrantedAuthority> collection = new ArrayList<>();

        collection.add(new GrantedAuthority() {
            @Override
            public String getAuthority() {
                return memberDTO.getRole();
            }
        });

        return collection;
    }

    @Override
    public String getName() {
        return memberDTO.getName();
    }

    public String getUsername() {
        return memberDTO.getUsername();
    }

    public Long getMemberId() {
        return memberDTO.getMemberId();
    }
}
