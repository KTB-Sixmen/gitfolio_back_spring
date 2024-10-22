package com.be.gitfolio.member.service;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import com.be.gitfolio.common.s3.S3Service;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import com.be.gitfolio.member.dto.MemberRequestDTO;
import com.be.gitfolio.member.dto.MemberResponseDTO;
import com.be.gitfolio.member.repository.MemberAdditionalInfoRepository;
import com.be.gitfolio.member.repository.MemberRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.Optional;

import static com.be.gitfolio.member.dto.MemberRequestDTO.*;
import static com.be.gitfolio.member.dto.MemberResponseDTO.*;

@Service
@RequiredArgsConstructor
@Slf4j
@Transactional(readOnly = true)
public class MemberService {

    private final MemberRepository memberRepository;
    private final MemberAdditionalInfoRepository memberAdditionalInfoRepository;
    private final S3Service s3Service;

    /**
     * 회원 생성
     */
    @Transactional
    public Long createMember(MemberCreateRequestDTO memberCreateRequestDTO) {
        // 회원 기본정보 생성 및 저장
        Member member = Member.from(memberCreateRequestDTO);
        Member savedMember = memberRepository.save(member);

        // 회원 추가정보 생성만
        MemberAdditionalInfo memberAdditionalInfo = MemberAdditionalInfo.from(savedMember.getId());
        memberAdditionalInfoRepository.save(memberAdditionalInfo);

        return savedMember.getId();
    }

    /**
     * username으로 회원 ID(PK) 조회
     */
    public Member findMemberIdByUsername(String username) {
        return memberRepository.findByUsername(username);
    }

//    /**
//     * 회원 추가 정보 생성
//     */
//    @Transactional
//    public String createMemberAdditionalInfo(Long memberId, MemberAdditionalRequestDTO memberAdditionalRequestDTO) {
//        Optional<MemberAdditionalInfo> memberAdditionalInfoOpt = memberAdditionalInfoRepository.findByMemberId(memberId.toString());
//
//        if (memberAdditionalInfoOpt.isPresent()) {
//            throw new BaseException(ErrorCode.ALREADY_EXIST_MEMBER_ADDITIONAL_INFO);
//        }
//
//        MemberAdditionalInfo memberAdditionalInfo = MemberAdditionalInfo.of(memberId, memberAdditionalRequestDTO);
//        MemberAdditionalInfo savedMemberAdditionalInfo = memberAdditionalInfoRepository.save(memberAdditionalInfo);
//        return savedMemberAdditionalInfo.getId();
//    }

//    /**
//     * 회원 추가 정보 수정
//     */
//    @Transactional
//    public void updateMemberAdditionalInfo(Long memberId, MemberAdditionalRequestDTO memberAdditionalRequestDTO) {
//        MemberAdditionalInfo memberAdditionalInfo = memberAdditionalInfoRepository.findByMemberId(memberId.toString())
//                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_ADDITIONAL_INFO));
//
//        memberAdditionalInfo.updateMemberAdditionalInfo(memberAdditionalRequestDTO);
//        memberAdditionalInfoRepository.save(memberAdditionalInfo);
//    }


    /**
     * 회원 정보 상세 조회
     */
    public MemberDetailDTO getMemberDetails(Long memberId) {
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        // MongoDB에서 추가 정보 조회
        Optional<MemberAdditionalInfo> additionalInfoOpt = memberAdditionalInfoRepository.findByMemberId(member.getId().toString());

        // 추가 정보가 없으면 기본값으로 처리
        MemberAdditionalInfo additionalInfo = additionalInfoOpt.orElseGet(() -> MemberAdditionalInfo.from(member.getId()));

        // DTO에 MySQL과 MongoDB 데이터를 함께 담아 반환
        return MemberDetailDTO.of(member, additionalInfo);
    }

    /**
     * 회원 기본 정보 수정
     */
    @Transactional
    public void updateMemberInfo(Long memberId, MemberUpdateRequestDTO memberUpdateRequestDTO, MemberAdditionalRequestDTO memberAdditionalRequestDTO, MultipartFile imageFile) throws IOException {
        // 기본 정보 수정
        Member member = memberRepository.findById(memberId)
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_INFO));

        String imageUrl = member.getAvatarUrl();
        if (!imageUrl.contains("avatars.githubusercontent.com")) {
            s3Service.deleteFile(imageUrl);
        }

        String avatarUrl = s3Service.uploadFile(imageFile);

        member.updateMember(memberUpdateRequestDTO, avatarUrl);
        memberRepository.save(member);

        // 추가 정보 수정
        MemberAdditionalInfo memberAdditionalInfo = memberAdditionalInfoRepository.findByMemberId(memberId.toString())
                .orElseThrow(() -> new BaseException(ErrorCode.NO_MEMBER_ADDITIONAL_INFO));

        memberAdditionalInfo.updateMemberAdditionalInfo(memberAdditionalRequestDTO);
        memberAdditionalInfoRepository.save(memberAdditionalInfo);
    }
}