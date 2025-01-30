import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { environment } from '../../../environments/environment';
import { Observable } from 'rxjs';

/**
 * Service for uploading and downloading files
 */
@Injectable({ providedIn: 'root' })
export class FileService {

  /**
   * Create a new instance
   */
  constructor(protected httpClient: HttpClient) {
  }

 /**
   * Upload a file
   */
  uploadFile(name: string, file: File): Observable<string> {
    const URL = environment.SERVICE_URL + 'file';

    const httpOptions = {
      headers: new HttpHeaders({
        'Content-Type': 'application/octet-stream',
        'filename': name
      }),
      responseType: 'text' as const
    };

    return this.httpClient.post(URL, file, httpOptions);
  }

  /**
   * Download the file by providing its physical path on the back-end. It is in the
   * responsibility of the caller to provide a valid path!
   */
  downloadFile(path: string): Observable<Blob>{
    const URL = environment.SERVICE_URL + 'file';

    console.log('Download file from path ' + path);

    const httpOptions = {
      headers: new HttpHeaders({
        'path': path
      }),
      responseType: 'blob' as 'json'
    };

    return this.httpClient.get<Blob>(URL, httpOptions);
  }

  /**
   * Open a file
   */
  openFile(data: Blob | string, fileName?: string) {
    const blob = new Blob([data], { type: 'application/octet-stream' });
    const url = window.URL.createObjectURL(blob);

    // Create a link dynamically in order to provide a reasonable file name
    const link = document.createElement('a');
    link.style.visibility = 'hidden';
    link.href = url;

    if (fileName) {
      link.download = fileName;
    } else {
      link.download = 'download.dat';
    }

    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  }

}
