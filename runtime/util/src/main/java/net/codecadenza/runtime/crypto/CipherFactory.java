/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.crypto;

import java.nio.charset.StandardCharsets;
import java.security.Key;
import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * <p>
 * Factory that provides encryption and decryption ciphers. Note that algorithms may not use padding as files are corrupt after
 * download.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CipherFactory {
	// Use AES encryption as it is much faster and provides a higher level of security compared to DES!
	private static final String ENCRYPTION_ALGORITHM = "AES";
	private static final String ENCRYPTION_TRANSFORM = "AES/CTR/NoPadding";
	private static final byte[] INIT_PARAM = { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
			0x0e, 0x0f };

	private final Key key;

	/**
	 * Constructor
	 * @param secret
	 */
	public CipherFactory(String secret) {
		this.key = new SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), ENCRYPTION_ALGORITHM);
	}

	/**
	 * @return an encryption cipher
	 * @throws Exception if the cipher could not be created
	 */
	public Cipher getEncryptionCipher() throws Exception {
		final var iv = new IvParameterSpec(INIT_PARAM);

		final Cipher encryptionCipher = Cipher.getInstance(ENCRYPTION_TRANSFORM);
		encryptionCipher.init(Cipher.ENCRYPT_MODE, key, iv);

		return encryptionCipher;
	}

	/**
	 * @return a decryption cipher
	 * @throws Exception if the cipher could not be created
	 */
	public Cipher getDecryptionCipher() throws Exception {
		final var iv = new IvParameterSpec(INIT_PARAM);

		final Cipher decryptionCipher = Cipher.getInstance(ENCRYPTION_TRANSFORM);
		decryptionCipher.init(Cipher.DECRYPT_MODE, key, iv);

		return decryptionCipher;
	}

}
