import platform
import os
import zipfile
import stat
import sys

from requests import request


def get_latest_build_artifacts_url():
    res = request('GET', 'https://dev.azure.com/buu342/6fa21e4f-4c4b-425c-891d-28537ab457a9/_apis/build/builds?$top=1&api-version=4.1')
    body = res.json()
    return os.path.join(body['value'][0]['url'], 'artifacts')

def main():
    destpath = sys.argv[1] if len(sys.argv) > 1 else '.'
    unfloader_zip_path = os.path.join(destpath, "UNFLoader.zip")
    os.makedirs(destpath, exist_ok=True)
    is_wsl = 'microsoft-standard' in str(platform.uname()).lower()
    unf_fn = 'UNFLoader.exe' if is_wsl else 'UNFLoader'
    artifact_url = get_latest_build_artifacts_url()

    # get all artifacts from most recent build
    artifacts_res = request('GET', f'{artifact_url}?api-version=4.1')
    artifacts = artifacts_res.json()['value']

    # get artifact url for current platform
    platform_artifact_url = None
    platform_name = 'windows' if is_wsl else 'linux'
    for artifact in artifacts:
        a_name = artifact['name']
        if platform_name in a_name:
            platform_artifact_url = artifact['resource']['downloadUrl']

    # download unf zipfile
    artifact_res = request('GET', platform_artifact_url)
    with open(unfloader_zip_path, 'wb') as unf_fp:
        unf_fp.write(artifact_res.content)

    # only extract the specific file that we need
    unfpath = None
    with zipfile.ZipFile(unfloader_zip_path, 'r') as zip_ref:
        for zipinfo in zip_ref.infolist():
            if not zipinfo.is_dir():
                unfpath = zip_ref.extract(zipinfo, destpath)

    unf_bin_path = os.path.join(destpath, unf_fn)
    # file gets extracted to [destpath]/unfloader-{platform}/UNFLoader[.exe],
    # so move binary to [destpath]/UNFLoader[.exe]
    os.rename(unfpath, unf_bin_path)
    # remove [destpath]/unfloader-{platform}/ directory
    os.rmdir(unfpath.rstrip(unf_fn))
    # remove UNFLoader.zip
    os.remove(unfloader_zip_path)
    
    # now need to add executable file permissions to unfloader 
    st = os.stat(unf_bin_path)
    os.chmod(unf_bin_path, st.st_mode | stat.S_IEXEC)

if __name__ == '__main__':
    main()
