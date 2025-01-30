import { Injectable } from '@angular/core';
import { Translation } from '../model/translation.interface';
import { TranslationMessageRepository } from './translation-message-repository';
import { environment } from '../../../environments/environment';

/**
 * Service for providing translation text fragments
 */
@Injectable({ providedIn: 'root' })
export class I18NService {
  private readonly translations: Array<Translation>;

  /**
   * Load translations
   */
  constructor() {
    this.translations = TranslationMessageRepository.getTranlationMessages(environment.LOCALE);
  }

  /**
   * Search for a translation via the given key. It is possible to add dynamic
   * content to the generated message. In this case the order of the parameters
   * must match the occurrences of the corresponding placeholders!
   */
  translate(key: string, ...values: string[]): string {
    // Search for the translation by using the provided key
    const translation = this.translations.find(item => item.key === key);

    // If the translation could not be found return with a system generated message!
    if (!translation) {
      console.error('No translation for ' + key + ' found!');
      return '???' + key.toUpperCase() + '???';
    }

    // Search for placeholders identified by '$(name)'
    const placeholders = this.getPlaceholders(translation.message);

    if (placeholders === null || placeholders.length === 0) {
      if (values.length > 0) {
        console.error(`There are values but no placeholders defined for translation key '${key}'!`);
      }

      return translation.message;
    } else if (values.length === 0) {
      console.error(`There are placeholders but no values defined for translation key '${key}'!`);

      return translation.message;
    } else if (placeholders.length !== values.length) {
      console.error(`Placeholders and provided values do not match for translation key '${key}'!`);

      // Simply return the message if both arrays have a different length!
      return translation.message;
    }

    let message = translation.message;

    // Replace the placeholders by the provided values
    for (let i = 0; i < placeholders.length; i++) {
      message = message.replace(placeholders[i], values[i]);
    }

    return message;
  }

  /**
   * Extract all placeholders from the given message
   */
  private getPlaceholders(message: string) {
    const regex = /\$\((\w+)\)/g;

    return message.match(regex);
  }

}
