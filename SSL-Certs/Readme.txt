
Some good links:
   https://realtimelogic.com/blog/2014/05/How-to-act-as-a-Certificate-Authority-the-Easy-Way
   https://datacenteroverlords.com/2012/03/01/creating-your-own-ssl-certificate-authority/


Once you've created a root CA certificate, install it into your browser as follows (Windows):
   - IE : Options / Content / Certificates 
   - Chrome : Settings / Search Settings / type "Certificates" / Manage Certificates
   - select Trusted Root Certification Authorities tab.
   - Click Import
   - Select the CA Bundle file
   - Accept the defaults for all options in the wizard.


